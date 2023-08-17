{
  pkgs ? import (fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/4fe8d07066f6ea82cda2b0c9ae7aee59b2d241b3.tar.gz";
    sha256 = "sha256:06jzngg5jm1f81sc4xfskvvgjy5bblz51xpl788mnps1wrkykfhp";
  }) {}
}:
pkgs.stdenv.mkDerivation rec {
  pname = "macro11";
  version = "0.1.0";

  src = pkgs.fetchgit {
    url = "https://github.com/nigeleke/macro11";
    sha256 = "sha256-vjbVDBn7wHGPnN97gMAryheHAe7vzvXqjrDiV6wHw/A=";
  };

  buildPhase = ''
    make
  '';

  installPhase = ''
    mkdir -p $out/bin
    mv macro11 $out/bin
    chmod +x obj2bin/obj2bin.pl
    mv obj2bin/obj2bin.pl $out/bin
  '';
}
