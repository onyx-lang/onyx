{
  stdenv,
  wasmer,
  lib,
}: let
  fs = lib.fileset;
  sourceFiles = [
    ./bin
    ./build.sh
    ./compiler
    ./core
    ./docs
    ./examples
    ./interpreter
    ./misc
    ./runtime
    ./scripts
    ./settings.sh
    ./shared
    ./tests
  ];
in
  fs.trace sourceFiles
  stdenv.mkDerivation {
    pname = "onyx";
    version = "0.1.13";

    src = fs.toSource {
      root = ./.;
      fileset = sourceFiles;
    };
    buildInputs = [wasmer];

    installPhase = ''
      runHook preInstall
      source ./settings.sh
      export ONYX_INSTALL_DIR=$out/
      ./build.sh compile install
      runHook postInstall
    '';
  }
