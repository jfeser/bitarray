{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, flake-utils, nixpkgs }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        ispc = if system == "aarch64-darwin" then
          (import ./nix/ispc-darwin.nix) pkgs
        else
          pkgs.ispc;
        ocamlPkgs = pkgs.ocaml-ng.ocamlPackages_4_14;
        defaultPackage = ocamlPkgs.buildDunePackage rec {
          pname = "bitarray";
          version = "0.1";
          useDune3 = true;
          minimalOCamlVersion = "4.14";
          nativeBuildInputs = [
            ocamlPkgs.base_quickcheck
            ocamlPkgs.core
            ocamlPkgs.core_bench
            pkgs.gcc
            ispc
          ];
          propagatedBuildInputs = [ ocamlPkgs.base ocamlPkgs.fmt ];
          src = ./.;
        };

      in {
        defaultPackage = defaultPackage;
        devShell = pkgs.mkShell {
          nativeBuildInputs =
            [ pkgs.ocamlformat pkgs.opam pkgs.ocamlPackages.ocaml-lsp ];
          inputsFrom = [ defaultPackage ];
        };
      });
}
