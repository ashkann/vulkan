{
    inputs = {
        nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
        flake-utils.url = "github:numtide/flake-utils";
    };

    outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
        let
            pkgs = import nixpkgs { inherit system; };
            ghc = pkgs.haskell.compiler.ghc967;
            clangGcc = pkgs.runCommand "clangGcc" {} ''
                mkdir -p $out/bin
                ln -s ${pkgs.clang}/bin/clang $out/bin/gcc
            '';
        in { 
            devShells.default = pkgs.mkShell { 
                buildInputs = [
                    pkgs.git
                    pkgs.stack
                    # clangGcc
                    pkgs.vulkan-headers
                    pkgs.vulkan-loader
                ];
            };
        });
}