let
  nixpkgs = import pins.nixpkgs {};

  pins = {
    # merge of https://github.com/NixOS/nixpkgs/pull/327219
    nixpkgs = builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/80ab71601549f1af09894ff006c7e368f05f6234.tar.gz";
      sha256 = "sha256:06mzgzplg85gxgvm43yxh1gkqsbnp5m5y8cvxlwzbzbpxq81jaq8";
    };

    # https://github.com/obsidiansystems/obelisk/pull/984
    # https://github.com/obsidiansystems/obelisk/pull/988
    # https://github.com/obsidiansystems/obelisk/pull/989
    # https://github.com/obsidiansystems/obelisk/pull/1075
    # https://github.com/obsidiansystems/obelisk/pull/1079
    # https://github.com/obsidiansystems/obelisk/pull/1080
    obelisk = nixpkgs.fetchFromGitHub {
      owner = "alexfmpe";
      repo = "obelisk";
      rev = "f0af584d57039e99176f3748186b6f1d6016139c";
      sha256 = "1dmdwb8fc1lswf9bz6093p0nbppv40ghc84ay4mrkbrl68wmcyp7";
    };

    # https://github.com/reflex-frp/reflex-dom/pull/470
    reflex-dom = nixpkgs.fetchFromGitHub {
      owner = "alexfmpe";
      repo = "reflex-dom";
      rev = "d7e04396d927bc98f988e9c3627f42c8fd750a03";
      sha256 = "sha256-FIj0unHF/6vquqbhYW9wVGfoqQ5fwNPbgqASKCZhj+c=";
    };

  };

  packages = self: with self; {};

  patches = {};

in { inherit nixpkgs packages patches pins; }
