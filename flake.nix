{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, flake-utils, nixpkgs }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlay = final: prev: {
          ispc = if system == "aarch64-darwin" then
            final.callPackage ./nix/ispc-darwin.nix { }
          else
            prev.ispc;

          ocamlPackages = prev.ocamlPackages.overrideScope' (ofinal: oprev: {
            bitarray = ofinal.buildDunePackage {
              pname = "bitarray";
              version = "0.1";
              useDune3 = true;
              minimalOCamlVersion = "4.13";
              nativeBuildInputs = [ final.ispc ];
              propagatedBuildInputs = with ofinal; [ base fmt ppx_jane ];
              src = ./.;
            };
          });
        };
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ overlay ];
        };

      in {
        overlays.default = overlay;
        defaultPackage = pkgs.ocamlPackages.bitarray;
        devShell = pkgs.mkShell {
          nativeBuildInputs = [
            pkgs.ocamlformat
            pkgs.opam
            pkgs.ocamlPackages.ocaml-lsp
            pkgs.ocamlPackages.core
            pkgs.ocamlPackages.base_quickcheck
          ];
          inputsFrom = [ self.defaultPackage.${system} ];
        };
      });
}
