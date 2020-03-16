<!-- Provide a clear overview of your changes. -->

I have:

  - [ ] Based work on latest `develop` branch
  - [ ] Looked for lint in my changes with `hlint .` (lint found code you did not write can be left alone)
  - [ ] Run the test suite: `$(nix-build -A selftest --no-out-link)`
  - [ ] (Optional) Run CI tests locally: `nix-build release.nix -A build.x86_64-linux --no-out-link` (or `x86_64-darwin` on macOS)
