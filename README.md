# Obelisk

Obelisk provides an easy way to develop and deploy your [Reflex](https://github.com/reflex-frp/reflex) project as web apps and as mobile apps.

- [Installing Obelisk](#installing-obelisk)
- [Developing an Obelisk project](#developing-an-obelisk-project)
  - [Hoogle](#hoogle)
  - [Adding Packages](#adding-packages)
  - [Adding Package Overrides](#adding-package-overrides)
- [Deploying](#deploying)
  - [Locally](#locally)
  - [EC2](#ec2)
  - [From macOS](#from-macos)
  - [Deploying an updated version](#deploying-an-updated-version)
- [Mobile](#mobile)
  - [iOS](#ios)
  - [Android](#android)

## Installing Obelisk
1. [Install Nix](https://nixos.org/nix/).
    If you already have Nix installed, make sure you have version 2.0 or higher.  To check your current version, run `nix-env --version`.
1. Set up nix caches
    1. If you are running NixOS, add this to `/etc/nixos/configuration.nix`:
        ```
        nix.binaryCaches = [ "https://cache.nixos.org/" "https://nixcache.reflex-frp.org" ];
        nix.binaryCachePublicKeys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" ];
        ```
        and rebuild your NixOS configuration (e.g. `sudo nixos-rebuild switch`).
    1. If you are using another operating system or linux distribution, ensure that these lines are present in your Nix configuration file (`/etc/nix/nix.conf` on most systems; [see full list](https://nixos.org/nix/manual/#sec-conf-file)):
        ```
        substituters = https://cache.nixos.org https://nixcache.reflex-frp.org
        trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=
        ```
        * other Linux: enable sandboxing (see these [issue172](https://github.com/obsidiansystems/obelisk/issues/172#issuecomment-411507818) or [issue6](https://github.com/obsidiansystems/obelisk/issues/6) if you run into build problems)
          ```
          sandbox = true
          ```
        * MacOS: disable sandboxing (there are still some impure dependencies for now)
          ```
          sandbox = false
          ```
          then restart the nix daemon
          ```
          sudo launchctl stop org.nixos.nix-daemon
          sudo launchctl start org.nixos.nix-daemon
          ```
1. Install obelisk: `nix-env -f https://github.com/obsidiansystems/obelisk/archive/master.tar.gz -iA command`

### Contributing to Obelisk

When developing on obelisk itself you may launch `ghcid` for the corresponding project as follows. For example to launch ghcid for `lib/backend` project:

```
nix-shell -A obeliskEnvs.obelisk-backend --run "cd lib/backend && ghcid -c 'cabal new-repl'"
```

Or to launch ghcid for `lib/command` project:

```
nix-shell -A obeliskEnvs.obelisk-command --run "cd lib/command && ghcid -c 'cabal new-repl'"
```

To re-install `ob` from source do
```
nix-env -f /path/to/obelisk -iA command
```

Note that `ob` will defer to the version found in your project's `.obelisk/impl` directory. To update that version specifically:

```shell
ob thunk unpack ./obelisk/impl
cd ./obelisk/impl
# apply your changes
```

If you want to commit your changes, first push them to your fork of obelisk and then

```shell
cd /your/project/root
ob thunk pack .obelisk/impl
git add .obelisk/impl
git commit -m "Bump obelisk"
```

### Accessing private repositories

To allow the Nix builder to access private git repositories, you must be set up
to access them via SSH. Follow these steps depending on the platform you need
access to:

- [GitHub](https://help.github.com/articles/connecting-to-github-with-ssh/)
- [GitLab](https://docs.gitlab.com/ee/gitlab-basics/create-your-ssh-keys.html)

## Developing an Obelisk project

To create a new Obelisk project, go to an empty directory and run:

```
ob init
```

Obelisk leverages ghcid to provide a live-reloading server that handles both frontend and backend. To run your Obelisk app and monitor the source for changes:

```
ob run
```

Now go to http://localhost:8000 (or the address/port specified in `config/common/route`) to access your app.

Every time you change the Haskell source files in frontend, common or backend, `ob run` will automatically recompile the modified files and reload the server. Furthermore, it will display on screen compilation errors and warnings if any.

### Hoogle

To enter a nix-shell from which you can run the hoogle command-line client or a hoogle server for your project:

`nix-shell -A shells.ghc --arg withHoogle true`

### Adding packages

In order to add package dependencies, declare them under the build-depends field in the appropriate cabal files (backend, common, and frontend each have their own). The corresponding Nix packages will automatically be selected when building.

### Adding package overrides

To add a version override to any Haskell package, or to add a Haskell package that doesn't exist in the nixpkgs used by Obelisk, use the `overrides` attribute in your project's `default.nix`. For example, to use a specific version of the `aeson` package fetched from GitHub and a specific version of the `waargonaut` package fetched from Hackage, your `default.nix` will look like:

```nix
# ...
project ./. ({ pkgs, ... }: {
# ...
  overrides = self: super: let
    aesonSrc = pkgs.fetchFromGitHub {
      owner = "obsidiansystems";
      repo = "aeson-gadt-th";
      rev = "ed573c2cccf54d72aa6279026752a3fecf9c1383";
      sha256 = "08q6rnz7w9pn76jkrafig6f50yd0f77z48rk2z5iyyl2jbhcbhx3";
    };
  in
  {
    aeson = self.callCabal2nix "aeson" aesonSrc {};
    waargonaut = self.callHackageDirect {
      pkg = "waargonaut";
      ver = "0.8.0.1";
      sha256 = "1zv28np3k3hg378vqm89v802xr0g8cwk7gy3mr77xrzy5jbgpa39";
    } {};
  };
# ...
```

For further information see [the Haskell section](https://nixos.org/nixpkgs/manual/#users-guide-to-the-haskell-infrastructure) of nixpkgs Contributors Guide.

## Deploying

### Locally

Build everything:

```
nix-build -A exe --no-out-link
```

Copy the result to a new directory, add configuration, and run!

```
mkdir test-app
ln -s $(nix-build -A exe --no-out-link)/* test-app/
cp -r config test-app
(cd test-app && ./backend)
```

### EC2

In this section we will demonstrate how to deploy your Obelisk app to an Amazon EC2 instance.

First create a new EC2 instance:

1. Launch a NixOS 18.09 EC2 instance (we recommend [this AMI](https://console.aws.amazon.com/ec2/v2/home?region=us-east-1#LaunchInstanceWizard:ami=ami-009c9c3f1af480ff3))
1. In the instance configuration wizard ensure that your instance has at least 1GB RAM and 10GB disk space.
1. When prompted save your AWS private key (`~/myaws.pem`) somewhere safe. We'll need it later during deployment.
1. Go to "Security Groups", select your instance's security group and under "Inbound" tab add a new rule for HTTP port 80 and 443.

At this stage your instance should be booting and become accessible shortly. Note down the hostname of your EC2 instance.

Now go to your Obelisk project directory (`~/code/myapp`), and initialize a deployment config (`~/code/myapp-deploy`):
Your project directory must be "thunkable", i.e. something on which `ob thunk pack` can be called. Usually it will be a git repository whose current revision has been pushed upstream.

```
cd ~/code/myapp
SERVER=ec2-35-183-22-197.ca-central-1.compute.amazonaws.com
ROUTE=https://myapp.com   # Publicly accessible route to your app
EMAIL=myname@myapp.com
ob deploy init \
  --ssh-key ~/myaws.pem \
  --hostname $SERVER \
  --route $ROUTE \
  --admin-email $EMAIL \
  ~/code/myapp-deploy
```

HTTPS is enabled by default; to disable https, pass `--disable-https` to the `ob deploy init` command above.

This step will also require that you manually verify the authenticity of the host `$SERVER`. Obelisk will save the fingerprint in a deployment-specific configuration. **Obelisk deployments do *not* rely on the `known_hosts` of your local machine.** This is because, in the event that you need to switch from one deploy machine / bastion host to another, you want to be absolutely sure that you're still connecting to the machines you think you are, even if that deploy machine / bastion host has never connected to them before. Obelisk explicitly avoids a workflow that encourages people to accept host keys without checking them, since that could result in leaking production secrets to anyone who manages to MITM you, e.g. via DNS spoofing or cache poisoning. (Note that an active attack is a circumstance where you may need to quickly switch bastion hosts, e.g. because the attacker has taken one down or you have taken it down in case it was compromised. In this circumstance you might need to deploy to production to fix an exploit or rotate keys, etc.) When you run `ob deploy` later it will rely on the saved verification in this step.

Next, go to the deployment directory that you just initialized and deploy!

```
cd ~/code/myapp-deploy
ob deploy push
```

`ob deploy push` will locally build your app and then transfer it, along with all the Nix package dependencies, via ssh to the EC2 instance. The backend will live in `/var/lib/backend`.

At this point you are done. Your app will be accessible at `${ROUTE}`. The currently deployed version - the git commit hash of the source repo - can be found at `${ROUTE}/version`.

### From macOS

Deploying from macOS requires some extra setup:

- [Install nix-darwin](https://github.com/LnL7/nix-darwin)
- [Install docker](https://docs.docker.com/)

Running `ob deploy push` will give you additional setup instructions.

### Deploying an updated version

If you'd like to deploy an updated version (with new commits) of your Obelisk app: simply go to the configuration directory, update the source thunk and push:

```
cd ~/code/myapp-deploy
ob deploy update
ob deploy push
```

## Mobile
Until Obelisk offers a `ob deploy` equivalent for mobile apps, you are recommended to do it manually as follows.

### iOS

#### First time setup
Development on iOS requires a computer running macOS and an iOS developer account.
Your account must also belong to an Apple Developer Team, if you want to access developer portal links (otherwise they'll redirect to your account page).

##### iPhone
1. Connect the iPhone on which you'd like to run builds - this will open up iTunes.
1. Click accept to authorize on both the computer and the iPhone.

##### Xcode
Install Xcode 8.2 (contains iOS SDK 10.2) and open it so that it runs its post install tool setup.

These versions will work out of the box but iOS SDKs prior to 11.3 should also work. You can choose another installed version in `default.nix`

More recent Xcodes should also work, as long as one of the SDKs mentioned above has been used.
To add another SDK to your current Xcode, [download](https://developer.apple.com/download/more/) the corresponding Xcode, extract it and copy its SDK folder next to the installed one, e.g.
```
open -W Xcode_9.2.xip
sudo cp -R Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS.sdk /Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS11.2.sdk
```


You can verify that you have correct versions by running
```
xcodebuild -showsdks
```

##### Certificates
To deploy and/or package apps, you'll need to inform Apple of your development devices and permissions by
adding credentials to the correct provisioning profile via the Apple Developer portal.

1. Open up XCode and go to Preferences - Accounts. Select the organization
Member role, click Manage Certificates, and add an iOS Development
certificate.
1. Go to [developer portal - devices](https://developer.apple.com/account/ios/device/) and add your device.
To find your device's UDID, select it in iTunes and click the serial number.
1. Go to [developer portal - development profiles](https://developer.apple.com/account/ios/profile/limited).
Create a development profile and add your certificate and device.
Click "Generate" and then download and open the profile.

#### Building
1. In your project's `default.nix` set values for `ios.bundleIdentifier` and `ios.bundleName`.
Ensure that `bundleIdentifier` matches the App ID of the development profile, or that you are using a wildcard profile.
1. Run `nix-build -A ios.frontend -o result-ios` to build the app. Find it at `result-ios/frontend.app`

#### Deploying
1. Connect the registered iPhone.
1. Find your Apple Team ID in the [developer portal](https://developer.apple.com/account/#/membership).
1. Run the deploy command with your Team ID:
```
result-ios/bin/deploy [TEAM_ID]
# or in debug mode via lldb:
result-ios/bin/deploy [TEAM_ID] -d
```

#### Packaging
1. Go to [developer portal - distribution profiles](https://developer.apple.com/account/ios/profile/production).
Create and download a distribution profile.
1. Run the package script with your TEAM ID and your distribution profile to create a `.ipa`:
```
result-ios/bin/package [TEAM_ID] /path/to/output/.ipa /path/to/profile/file
```

#### Debugging
It's also possible to inspect iOS WkWebView apps once they are installed in the iPhone:
1. On the desktop, go to Safari > Preferences > Advanced and enable Develop menu.
1. On the iPhone go to Settings > Safari > Advanced and enable Web Inspector.
1. Open the app on the iPhone while it is connected to the desktop.
1. In the desktop's Safari Develop menu, you should see your iPhone. Select the screen under the name of the app.

### Android

NOTE: Currently Android builds are only supported on Linux.

1. In your project's `default.nix` set a suitable value for `android.applicationId` and `android.displayName`.
1. In your project's `default.nix` pass `config.android_sdk.accept_license = true;` in the arguments to the import of of `.obelisk/impl` to indicate your acceptance of the [Android Software Development Kit License Agreement](https://developer.android.com/studio/terms), which is required to build Android apps.
1. Run `nix-build -A android.frontend -o result-android` to build the Android app.
1. A debug version of the app should be generated at `result-android/android-app-debug.apk`

Now deploy the built apk file to your Android device:

1. Enable *USB debugging* in your Android device ([instructions here](https://developer.android.com/studio/debug/dev-options))
1. Connect the device using USB (be sure to confirm any security prompts on the device)
1. Run the deploy script: `result-android/bin/deploy`

Alternatively, you can deploy from an obelisk deployment directory (a directory generated post `ob deploy init ...` command) using the `ob deploy test android` command.
This command will accomplish the following:

1. Create a key store and apk signing key (`android_keystore.jks`)
1. Build a Signed Android apk for your application
1. Deploy the Signed apk to your connected Android device

In the event that you change your key or keystore password, you will have to update your credentials within the JSON object found in `android_keytool_config.json`.

Additional documentation on Java key stores can be found [here](https://docs.oracle.com/javase/8/docs/technotes/tools/unix/keytool.html).

This should copy over and install the application on your device (if you see a  "*signatures do not match*" error, simply uninstall the previous app from the device before retrying the deploy). The name of the installed application will be what you have specified for `android.displayName` in the `default.nix`.

#### Releasing to Play Store

##### Build a release version

After having configured signing for your app, you may proceed to build a release version of the app. This is no different to how you build the non-release version, so consult the section [Android](#android) further above for exact instructions on building and deploying to your device.
