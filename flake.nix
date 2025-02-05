{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };
  outputs = {
    self,
    nixpkgs,
    ...
  }: let
    systems = nixpkgs.lib.systems.flakeExposed;
  in {
    formatter = nixpkgs.lib.genAttrs systems (
      system: let
        pkgs = nixpkgs.legacyPackages.${system};
      in
        pkgs.alejandra
    );
    packages = nixpkgs.lib.genAttrs systems (system: let
      pkgs = nixpkgs.legacyPackages.${system};
      version = "0.0.1"; # TODO: Where to extract version from?
      suffix =
        if pkgs.stdenv.isLinux
        then "so"
        else if pkgs.stdenv.isDarwin
        then "dylib"
        else "dll"; # TODO: Is it even possible for Windows? Use targetPlatform.isWindows?
      shared = pkgs.stdenv.mkDerivation {
        name = "shared";
        src = ./shared;
        installPhase = ''
          mkdir -p $out/include
          cp -r ./include/* $out/include/
        '';
      };
      compiler-types = pkgs.stdenv.mkDerivation {
        # TODO: Maybe move ./compiler/types.h to ./shared?
        name = "compiler-types";
        src = ./compiler;
        installPhase = ''
          mkdir -p $out/include
          cp -r ./include/types.h $out/include/
        '';
      };
      build-interpreter = {
        debug ? false,
        verbose ? false,
      }: let
        files = map (f: "src/${f}.c") ["ovmwasm" "vm/*" "wasm/*" "debug/*"];
        debug-flags =
          if debug
          then ["-g3" "-O2" "-DOVM_DEBUG=1"]
          else ["-O3"];
        verbose-flags =
          if verbose
          then ["-g3" "-DOVM_VERBOSE=1"]
          else [];
        flags = ["-fno-stack-protector" "-fPIC"] ++ debug-flags ++ verbose-flags;
        includes = map (i: "-I${i}") ["./include" "${pkgs.stb}/include/stb"];
        # libs = "-pthread"; # TODO: Check if necessary
        FILES = builtins.concatStringsSep " " files;
        FLAGS = builtins.concatStringsSep " " flags;
        INCLUDES = builtins.concatStringsSep " " includes;
      in
        pkgs.stdenv.mkDerivation {
          name = "ovmwasm";
          src = ./interpreter;
          buildInputs = [shared];
          buildPhase = ''
            for file in ${FILES}; do
              $CC ${FLAGS} ${INCLUDES} -c $file -o $TMP/$(basename $file).o
            done
            ar crs lib$name.a $TMP/*.o
          '';
          installPhase = ''
            mkdir -p $out/lib
            cp lib$name.a $out/lib/
          '';
        };
      build-compiler = {
        debug ? false,
        verbose ? false,
        runtime ? null,
        dyncall ? false,
      }: let
        runtime-files =
          if (runtime != null)
          then ["wasm_runtime"]
          else [];
        files = map (f: "src/${f}.c") ([
            "library_main"
            "astnodes"
            "builtins"
            "checker"
            "clone"
            "doc"
            "entities"
            "errors"
            "lex"
            "parser"
            "types"
            "utils"
            "wasm_emit"
            "extensions"
          ]
          ++ runtime-files);
        common-flags = [
          "-DENABLE_DEBUG_INFO"
          "-fvisibility=hidden"
          "-fPIC"
        ];
        warning-flags = map (w: "-W${w}") [
          "implicit"
          "misleading-indentation"
          "parentheses"
          "sequence-point"
          "return-type"
          "shift-negative-value"
          "unused-but-set-parameter"
          "unused-but-set-variable"
          "unused-function"
          "unused-label"
          "sign-compare"
          "strict-overflow"
          "trigraphs"
          "address"
        ];
        runtime-flags =
          if (runtime != null)
          then ["-DONYX_RUNTIME_LIBRARY=${runtime}"]
          else [];
        ovmwasm-flags =
          if runtime == "ovmwasm"
          then ["-DUSE_OVM_DEBUGGER"]
          else [];
        debug-flags =
          if debug
          then ["-g3"]
          else ["-O3"];
        dyncall-flags =
          if (dyncall && runtime == "ovmwasm")
          then ["-DUSE_DYNCALL"]
          else [];
        flags = common-flags ++ warning-flags ++ debug-flags ++ runtime-flags ++ ovmwasm-flags ++ dyncall-flags;
        platform-libs =
          if pkgs.stdenv.isDarwin
          then ["-lffi" "-framework CoreFoundation" "-framework SystemConfiguration"]
          else [];
        platform-pkgs = builtins.attrValues (
          if pkgs.stdenv.isDarwin
          then {
            inherit (pkgs) libffi;
            inherit (pkgs.darwin.apple_sdk.frameworks) CoreFoundation SystemConfiguration;
          }
          else {}
        );
        runtime-pkg =
          if runtime == "ovmwasm"
          then (build-interpreter {inherit debug verbose;})
          # else if runtime == "wasmer" #TODO: Make wasmer work!
          # then pkgs.wasmer
          else null;
        runtime-libs =
          if runtime == "ovmwasm"
          then ["-l${runtime}"]
          else [];
        dyncall-pkg =
          if (dyncall && runtime == "ovmwasm")
          then pkgs.dyncall
          else null;
        dyncall-libs =
          if (dyncall && runtime == "ovmwasm")
          then ["-ldyncall_s" "-ldyncallback_s"]
          else [];
        libs = ["-lpthread" "-ldl" "-lm"] ++ runtime-libs ++ dyncall-libs ++ platform-libs;
        includes = map (i: "-I${i}") ["./include" "${pkgs.stb}/include/stb"];
        FILES = builtins.concatStringsSep " " files;
        FLAGS = builtins.concatStringsSep " " flags;
        LIBS = builtins.concatStringsSep " " libs;
        INCLUDES = builtins.concatStringsSep " " includes;
      in
        pkgs.stdenv.mkDerivation {
          name = "onyx";
          src = ./compiler;
          buildInputs = [shared runtime-pkg dyncall-pkg] ++ platform-pkgs;
          buildPhase = ''
            for file in ${FILES}; do
              $CC ${FLAGS} ${INCLUDES} -c $file -o $(basename $file).o
            done
            $CC ${FLAGS} ${INCLUDES} cli/main.c *.o -o $name ${LIBS}
            $CC -shared -o lib$name.${suffix} *.o ${LIBS}
          '';
          installPhase = ''
            mkdir -p $out/bin
            cp $name $out/bin/
            mkdir -p $out/lib
            cp -r lib$name.${suffix} $out/lib/
          '';
        };
      build-runtime = {}: let
        flags = ["-fPIC" "-O2" "-Wno-incompatible-pointer-types"];
        platform-pkgs = builtins.attrValues (
          if pkgs.stdenv.isDarwin
          then {inherit (pkgs.darwin.apple_sdk.frameworks) Security;}
          else {}
        );
        platform-libs =
          if pkgs.stdenv.isDarwin
          then ["-framework Security"]
          else [];
        libs = ["-lpthread"] ++ platform-libs;
        FLAGS = builtins.concatStringsSep " " flags;
        LIBS = builtins.concatStringsSep " " libs;
      in
        pkgs.stdenv.mkDerivation {
          name = "onyx_runtime";
          src = ./runtime;
          buildInputs = [shared compiler-types] ++ platform-pkgs;
          buildPhase = ''
            $CC -shared ${FLAGS} -o $name.${suffix} ./$name.c ${LIBS}
          '';
          installPhase = ''
            mkdir -p $out/
            cp $name.${suffix} $out/
          '';
        };
      build-package = {
        debug ? false,
        verbose ? false,
        runtime ? null,
        dyncall ? false,
      }: let
        compiler = build-compiler {inherit debug verbose runtime dyncall;};
        runtime-lib = build-runtime {};
        platform-pkgs = builtins.attrValues (
          if pkgs.stdenv.isDarwin
          then {inherit (pkgs.darwin) autoSignDarwinBinariesHook;} # This is used to `codesign`
          else {}
        );
      in
        pkgs.stdenv.mkDerivation {
          name = "onyx";
          inherit version;
          src = pkgs.lib.fileset.toSource {
            root = ./.;
            fileset = pkgs.lib.fileset.unions [
              ./core
              ./scripts
              ./examples
              ./misc
              ./LICENSE
            ];
          };
          nativeBuildInputs = [pkgs.makeWrapper] ++ platform-pkgs;
          buildInputs = [compiler runtime-lib shared];
          buildPhase = ''
            # Install core libs
            mkdir -p $out/core
            cp -r ./core $out/
            # Install compiler
            mkdir -p $out/{bin,lib,include}
            cp ${compiler}/bin/onyx $out/bin/
            cp ${compiler}/lib/libonyx.${suffix} $out/lib/
            cp ${shared}/include/onyx.h $out/include/
            # Install tools
            mkdir -p $out/{tools,tools/pkg_templates}
            cp ./scripts/onyx-pkg.onyx $out/tools/
            cp ./scripts/default.json $out/tools/pkg_templates/
            cp ./scripts/lsp.wasm $out/tools/
            # Install runtime library if enabled
            ${
              if runtime != null
              then ''
                cp ${runtime-lib}/onyx_runtime.${suffix} $out/lib/
                cp ${shared}/include/onyx_library.h $out/include/
                cp ${shared}/include/wasm.h $out/include/
              ''
              else ""
            }
            # Copy examples
            mkdir -p $out/examples
            cp -r examples $out
            # Copy misc files
            mkdir -p $out/misc
            cp misc/onyx-linux.sublime-build $out/misc/
            cp misc/onyx-windows.sublime-build $out/misc/
            cp misc/onyx-mode.el $out/misc/
            cp misc/onyx.sublime-syntax $out/misc/
            cp misc/onyx.tmPreferences $out/misc/
            cp misc/vscode/onyxlang-0.1.9.vsix $out/misc/
            # Copy license
            cp LICENSE $out/
          '';
          postFixup = ''
            wrapProgram $out/bin/onyx --set ONYX_PATH $out
          '';
          meta = {
            description = "The Onyx Programming Language";
            homepage = "https://onyxlang.io";
            license = pkgs.lib.licenses.bsd2;
            platforms = pkgs.lib.platforms.all;
          };
        };
      build-compressed = {src}:
        pkgs.stdenv.mkDerivation {
          name = "compressed";
          inherit version;
          inherit src;
          buildPhase = ''
            mkdir -p $out
            tar -C ${src} -zcvf $out/$name-$version.tar.gz *
          '';
        };
    in rec {
      default = build-package {
        runtime = "ovmwasm";
        dyncall = false;
      };
      onyx = default;
      compressed = build-compressed {src = default;};
    });
    devShells = nixpkgs.lib.genAttrs systems (system: let
      pkgs = nixpkgs.legacyPackages.${system};
      pkgs-self = self.packages.${system};
    in {
      default = pkgs.mkShell {
        packages = builtins.attrValues {
          inherit (pkgs) nixd nil alejandra wasmer;
          inherit (pkgs-self) onyx;
        };
      };
    });
  };
}
