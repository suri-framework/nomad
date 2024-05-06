{
  description = "Minimal composable server framework for Riot.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    atacama = {
      url = "github:suri-framework/atacama";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.riot.follows = "riot";
      inputs.telemetry.follows = "telemetry";
    };

    trail = {
      url = "github:suri-framework/trail";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.atacama.follows = "atacama";
      inputs.riot.follows = "riot";
    };

    riot = {
      url = "github:riot-ml/riot";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.telemetry.follows = "telemetry";
    };

    telemetry = {
      url = "github:leostera/telemetry";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];
      perSystem = { config, self', inputs', pkgs, system, ... }:
        let
          inherit (pkgs) ocamlPackages mkShell;
          inherit (ocamlPackages) buildDunePackage;
          version = "0.0.1+dev";
        in
          {
            devShells = {
              default = mkShell {
                buildInputs = with ocamlPackages; [
                  dune_3
                  ocaml
                  utop
                  ocamlformat
                ];
                inputsFrom = [ self'.packages.default ];
                packages = builtins.attrValues {
                  inherit (ocamlPackages) ocaml-lsp ocamlformat-rpc-lib;
                };
              };
            };

            packages = {
              default = buildDunePackage {
                inherit version;
                pname = "nomad";
                propagatedBuildInputs = with ocamlPackages; [
                  inputs'.atacama.packages.default
                  bitstring
                  decompress
                  digestif
                  httpaf
                  ppx_bitstring
                  inputs'.riot.packages.default
                  inputs'.telemetry.packages.default
                  inputs'.trail.packages.default
                  uutf
                ];
                src = ./.;
              };
            };
          };
    };
}
