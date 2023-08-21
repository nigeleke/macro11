{
  description = "Macro-11 assembler and obj2bin utility.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/master";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      default = self.mkDerivation {
        name = "macro11";
        src = ./.;
        buildInputs = [ pkgs.gnumake ];
        makeFlags = [ "-D SKIP_GIT_INFO" ];
        installPhase = ''
          mkdir -p $out/bin
          install macro11 $out/bin
        '';
      };
    };

}
