{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, flake-utils, nixpkgs }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        ocamlPkgs = pkgs.ocaml-ng.ocamlPackages;
        ispc = if system == "aarch64-darwin" then
          pkgs.callPackage ./nix/ispc-darwin.nix { }
        else
          pkgs.ispc;

        bitarray = ocamlPkgs.buildDunePackage {
          pname = "bitarray";
          version = "0.1";
          useDune3 = true;
          minimalOCamlVersion = "4.13";
          nativeBuildInputs = [ ispc ];
          propagatedBuildInputs = with ocamlPkgs; [ base fmt ppx_jane ];
          src = ./.;
        };
      in {
        defaultPackage = bitarray;
        devShell = pkgs.mkShell {
          nativeBuildInputs = [
            pkgs.ocamlformat
            pkgs.opam
            pkgs.ocamlPackages.ocaml-lsp
            pkgs.ocamlPackages.core
            pkgs.ocamlPackages.base_quickcheck
          ];
          inputsFrom = [ bitarray ];
        };
      });
}
