hackGet: rp: 
    let
      rp810 = rp // { ghc = rp.ghc8_10; ghcjs = rp.ghcjs8_10; };
    in
    rp810 // { project = args: import (hackGet (./dep/reflex-platform) + "/project") rp810 (args ({ pkgs = rp.nixpkgs; } // rp810)); }
