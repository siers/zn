{
  description = "A very basic flake";

  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixos-20.03;

  outputs = { self, nixpkgs }: {
    defaultPackage.x86_64-linux = (import nix/default.nix {
      nixpkgs = import nixpkgs { system = "x86_64-linux"; };
    });
  };
}
