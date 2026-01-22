{
  stdenv,
  wasmer,
  lib,
}: let
  fs = lib.fileset;
  sourceFiles =
    fs.difference
    ./.
    (fs.unions [
      (fs.fileFilter (file: file.hasExt "nix") ./.)
      (fs.fileFilter (file: file.hasExt "bat") ./.)
    ]);
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
