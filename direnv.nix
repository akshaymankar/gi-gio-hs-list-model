{ ghc ? "ghc8104" }:
let
  pkgs = import ./nix;

  hls = pkgs.haskellPackages.haskell-language-server;

  deps = [
      # Tools
      pkgs.haskell.compiler.${ghc}
      pkgs.haskellPackages.cabal-install

      # For cabal
      pkgs.pkgconfig
      pkgs.binutils

      pkgs.zlib.dev
      pkgs.zlib.out
      pkgs.gobject-introspection.dev
      pkgs.glib.dev
  ];
  devDeps = deps ++ [ hls ];
in {
  devEnv = pkgs.buildEnv {
    name = "gi-gio-hs-list-model";
    paths = devDeps;
  };
  direnv = pkgs.buildEnv { name ="direnv"; paths = [pkgs.direnv]; };
}

