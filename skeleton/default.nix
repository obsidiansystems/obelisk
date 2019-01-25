{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
, closureCompilerOptimizationLevel ? "ADVANCED" # Set this to `null` to skip the closure compiler step
}:
with import ./.obelisk/impl { inherit system iosSdkVersion; };
project ./. ({ ... }: {
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";
  __closureCompilerOptimizationLevel = closureCompilerOptimizationLevel;
})
