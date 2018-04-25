{ mkDerivation, base, classy-prelude, directory, extra
, optparse-applicative, process, stdenv
}:
mkDerivation {
  pname = "ezmon";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base classy-prelude directory extra optparse-applicative process
  ];
  license = stdenv.lib.licenses.unfree;
  hydraPlatforms = stdenv.lib.platforms.none;
}
