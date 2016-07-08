with import <nixpkgs> { };

stdenv.mkDerivation {
  name = "stack-nix";
  src = ./.;
  buildInputs = [
    (haskell.packages.lts-6_3.ghcWithPackages (p: [ p.stack ]))
  ];
  buildPhase = ''
    ghc -Wall -o stack-nix Main.hs
  '';
  installPhase = ''
    mkdir -p $out/bin
    mv stack-nix $out/bin/
  '';
}
