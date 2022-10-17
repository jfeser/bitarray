{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.05";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, flake-utils, nixpkgs }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        ispc = if system == "aarch64-darwin" then
          pkgs.callPackage ./nix/ispc-darwin.nix { }
        else
          pkgs.ispc;

        ocamlPkgs = pkgs.ocaml-ng.ocamlPackages;
        defaultPackage = ocamlPkgs.buildDunePackage rec {
          pname = "bitarray";
          version = "0.1";
          useDune3 = true;
          minimalOCamlVersion = "4.13";
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
