{ lib
, stdenv
, fetchgit
, macro11
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "macro11";
  version = "0.1.0";

  src = fetchgit {
    url = "https://github.com/nigeleke/macro11";
    sha256 = "sha256-IcVUCBvTB/HtLdzd1wGhmUbvbvvQUf2Kpgcb1oQOkb4=";
  };

  buildPhase = ''
    make CFLAGS="-D SKIP_GIT_INFO=1"
  '';

  installPhase = ''
    mkdir -p $out/bin
    install macro11 $out/bin
    install obj2bin/obj2bin.pl $out/bin
  '';

  meta = with lib; {
    description = "Macro-11 assembler and obj2bin utility.";
    longDescription = ''
      macro11 is an assembler for the PDP-11, or PiDP-11 / SIMH simulator.
      obj2bin.pl is a utility that creates bin files from obj files, to be loaded via boot.ini.
    '';
    platforms = platforms.all;
  };
})
