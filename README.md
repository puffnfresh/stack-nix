Experimenting with building a Stack project's dependencies using Nix, then registering them in the Stack package database.

## Usage

Make sure your project's correct version of GHC is on the path.

    # Figures out the Stack dependencies and creates a shell with those packages in GHC.
    stack-nix > shell.nix
    # Registers the Nix GHC packages in the Stack package database.
    nix-shell --command 'eval "$registerStackPackages"'
    # Creates flag cache files, so Stack will use the registered packages.
    stack-nix
