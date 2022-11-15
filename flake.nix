{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, flake-utils, nixpkgs }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ self.overlay.${system} ];
        };
      in {
        overlay = self: super:
          let
            ispc = if system == "aarch64-darwin" then
              super.callPackage ./nix/ispc-darwin.nix { }
            else
              self.ispc;
            gcc = self.gcc;
          in {
            ocamlPackages = super.ocamlPackages.overrideScope' (self: super: {
              bitarray = super.buildDunePackage {
                pname = "bitarray";
                version = "0.1";
                useDune3 = true;
                minimalOCamlVersion = "4.13";
                nativeBuildInputs =
                  [ self.base_quickcheck self.core self.core_bench gcc ispc ];
                propagatedBuildInputs = [ self.base self.fmt ];
                src = ./.;
              };
            });
          };
        defaultPackage = pkgs.ocamlPackages.bitarray;
        devShell = pkgs.mkShell {
          nativeBuildInputs =
            [ pkgs.ocamlformat pkgs.opam pkgs.ocamlPackages.ocaml-lsp ];
          inputsFrom = [ pkgs.ocamlPackages.bitarray ];
        };
      });
}
