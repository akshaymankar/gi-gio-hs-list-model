self: super: {
  haskellPackages = super.haskellPackages.override({
    overrides = hself: hsuper: {
      # HLS segfaults for GTK things without this.
      haskell-language-server = self.haskell.lib.appendConfigureFlag hsuper.haskell-language-server "--enable-executable-dynamic";
    };
  });
}
