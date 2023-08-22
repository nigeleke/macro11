{
  description = "Macro-11 assembler and obj2bin utility.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/master";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in {
        packages = {
          default = pkgs.stdenv.mkDerivation {
	    name = "macro11";
	    src = ./.;

            nativeBuildInputs = with pkgs; [
		gnumake
		vscode
	    ];

	    buildPhase = ''
	      make CFLAGS="-D SKIP_GIT_INFO"
	    '';

	    installPhase = ''
	      mkdir -p $out/bin
	      install macro11 $out/bin
	      install obj2bin/obj2bin.pl $out/bin
              install obj2bin/obj2bin.sh $out/bin
	    '';

	  };
        };
     });
}
