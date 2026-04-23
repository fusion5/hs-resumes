{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    # retrieved using https://www.nixhub.io/packages/haskellPackages.cabal-fmt
    nixpkgs-cabal-fmt.url = "github:nixos/nixpkgs/09061f748ee21f68a089cd5d91ec1859cd93d0be";
  };
  outputs = inputs @ {
    self,
    nixpkgs,
    flake-parts,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [inputs.haskell-flake.flakeModule];

      perSystem = {
        self',
        system,
        pkgs,
        ...
      }: {
        haskellProjects.default = {
          # The base package set representing a specific GHC version.
          # By default, this is pkgs.haskellPackages.
          # You may also create your own. See https://community.flake.parts/haskell-flake/package-set
          basePackages = pkgs.haskell.packages.ghc9122;

          # Extra package information. See https://community.flake.parts/haskell-flake/dependency
          #
          # Note that local packages are automatically included in `packages`
          # (defined by `defaults.packages` option).
          #
          packages = {
            # aeson.source = "1.5.0.0";      # Override aeson to a custom version from Hackage
            # shower.source = inputs.shower; # Override shower to a custom source path
          };
          settings = {
            sandwich = {
              check = false;
            };
            all = {
              haddock = true;
            };
          };

          devShell = {
            # Enabled by default
            enable = true;
            hoogle = true;

            # Programs you want to make available in the shell.
            # Default programs can be disabled by setting to 'null'
            tools = hp: {
              fourmolu = hp.fourmolu;
              # ghcid = null;
              haskell-language-server = hp.haskell-language-server;
              cabal-plan = hp.cabal-plan;
              pkg-config = pkgs.pkg-config;
              # the cabal-fmt from 25.11 doesn't support ghc 9.12, so we override it
              cabal-fmt = inputs.nixpkgs-cabal-fmt.legacyPackages.${system}.haskellPackages.cabal-fmt;
              implicit-hie = hp.implicit-hie; # Used to generate hie.yaml for the haskell-language-server
            };

            # Check that haskell-language-server works
            hlsCheck.enable = false; # Requires sandbox to be disabled

            # Add extra parameters to the underlying mkShell call
            mkShellArgs = {
              CABAL_BUILDDIR = "/tmp/hs-resumes-cabal-build";
              nativeBuildInputs = [
                pkgs.zlib
                pkgs.hpack
              ];
              shellHook = ''
                echo "Welcome to the development REPL"

                mkdir -vp $CABAL_BUILDDIR
                if [ ! -d "dist-newstyle" ] && [ ! -L "dist-newstyle" ]; then
                  echo "Creating symbolic link: dist-newstyle -> $CABAL_BUILDDIR"
                  ln -s "$CABAL_BUILDDIR" dist-newstyle
                else
                  echo "dist-newstyle already exists or is a link. Skipping link creation."
                fi
                ${pkgs.hpack}/bin/hpack
              '';
            };
          };
        };
      };
    };
}
