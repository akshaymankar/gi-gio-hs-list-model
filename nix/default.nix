let
  sources = import ./sources.nix;
  pkgs = import sources.nixpkgs {
    overlays = [
      (_: _: { niv = (import sources.niv {}).niv; })
      (import ./overlays/hls.nix)
    ];
  };
in
  pkgs
