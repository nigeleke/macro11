# Macro11

## Development

Prerequisites:

- Install nix
- Allow [unfree](https://nixos.wiki/wiki/Unfree_Software) for vscode

Development environment is provided with the vscode IDE / editor.

```
nix develop --impure
```

Build macro11 using the `make` command.

```
make
```

## Build Nix package

```
nix build --impure
```

## Usage in another devShell

`flake.nix`:


```
{
  inputs = {
    macro11pkg.url = "github:nigeleke/macro11";
  };

  outputs = { macro11pkg }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      devShells.${system}.default = pkgs.mkShell {
        packages = [ macro11pkg.packages.${system}.default ];
      };
    };
}
```