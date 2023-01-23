{
  inputs =
    {
      nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
      ps-tools.follows = "purs-nix/ps-tools";
      purs-nix.url = "github:purs-nix/purs-nix/ps-0.15";
      utils.url = "github:numtide/flake-utils";
      npmlock2nix =
        {
          flake = false;
          url = "github:nix-community/npmlock2nix";
        };
    };

  outputs = { nixpkgs, utils, ... }@inputs:
    utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ]
      (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          ps-tools = inputs.ps-tools.legacyPackages.${system};
          purs-nix = inputs.purs-nix { inherit system; };

          ps =
            purs-nix.purs
              {
                dependencies =
                  with purs-nix.ps-pkgs;
                  [
                    argonaut
                    argonaut-codecs
                    console
                    effect
                    affjax-node
                    node-fs-aff
                    node-path
                    optparse
                    prelude
                    stringutils
                  ];

                test-dependencies =
                  with purs-nix.ps-pkgs;
                  [
                    debug
                    spec
                    spec-discovery
                    spec-quickcheck
                  ];

                dir = ./.;

                foreign.xhr2.node_modules =
                  npmlock2nix.v1.node_modules { src = ./.; };
              };

          npmlock2nix = import inputs.npmlock2nix { inherit pkgs; };

        in
        {
          packages.default = ps.modules.Main.bundle { };

          devShells.default =
            pkgs.mkShell
              {
                packages =
                  with pkgs;
                  [
                    entr
                    nodejs-14_x
                    nodePackages.purs-tidy
                    (ps.command { })
                    ps-tools.for-0_15.purescript-language-server
                    purs-nix.esbuild
                    purs-nix.purescript
                    (((import ./analysis.nix) { inherit pkgs; })).out
                  ];

                shellHook =
                  ''
                    alias watch="find src | entr -s 'echo bundling; purs-nix bundle'"
                  '';
              };
        }
      );
}
