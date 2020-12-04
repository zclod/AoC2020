with (import <nixpkgs> {});
mkShell {
  buildInputs = [ (haskellPackages.ghcWithPackages (p: [p.protolude p.containers p.bytestring p.text p.megaparsec p.turtle p.recursion-schemes]))  ];
}
