self: super: {
  haskellPackages = super.haskellPackages.extend (import ./haskell-overlay.nix self super);
}
