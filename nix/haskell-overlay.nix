pkgsSelf: pkgsSuper: haskellSelf: haskellSuper: {
  xml-clark = haskellSuper.callCabal2nix "xml-clark" ../. {};
}
