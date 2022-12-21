# Obelisk

[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)](http://www.haskell.org)
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)](https://github.com/obsidiansystems/obelisk/blob/master/LICENSE)

<p align="center"><img src="docs/obelisk-logo-640.png" width="50%" alt="Obelisk Logo"></p>

Functional reactive web and mobile applications, with batteries included. Obelisk's goal is to represent a cohesive, highly-curated set of choices that [Obsidian Systems](https://obsidian.systems/) has made for building these types of applications in a way that is extremely fast but does not compromise on production readiness.

- [Overview](#overview)
  - [Who should consider using it?](#who-should-consider-using-it)
- [Installing Obelisk](#installing-obelisk)
- [Developing an Obelisk project](#developing-an-obelisk-project)
  - [Local Hoogle](#local-hoogle)
  - [Adding Packages](#adding-packages)
  - [Adding Package Overrides](#adding-package-overrides)
  - [Running tests](#running-tests)
  - [Running over HTTPS](#running-over-https)
  - [IDE Support](#ide-support)
- [Deploying](#deploying)
  - [Default EC2 Deployment](#default-ec2-deployment)
  - [Custom Non-EC2 Deployment](#custom-non-ec2-deployment)
    - [VirtualBox Deployment](#virtualbox-deployment)
  - [Locally](#locally)
  - [From macOS](#from-macos)
  - [Deploying an updated version](#deploying-an-updated-version)
- [Mobile](#mobile)
  - [iOS](#ios)
  - [Android](#android)
- [Frequently Asked Questions (FAQ)](#frequently-asked-questions-faq)
- [Contributing](#contributing)

## Overview

Obelisk allows you to build high-quality web and mobile applications very quickly using [Reflex](https://reflex-frp.org/). In minutes you can go from an empty directory to an interactive application that works on web, iOS, and Android, all sharing the same Haskell codebase! Obelisk's development environment also enables extremely rapid development and feedback. You can take advantage of Haskell's type system across the frontend and backend boundary. This means changes to your backend that would break your frontend are immediately detected during development and vice versa. Obelisk uses Haskell's compiler to give you a complete "TODO list" of what needs to be updated.

Obelisk is targeted primarily at Haskell developers who want to build high-quality web and/or mobile applications in Haskell, without the distractions of manually choosing and integrating technology for every piece of the system.

### Who should consider using it?

Obelisk assumes basic knowledge of [Haskell](https://www.haskell.org/) and [Reflex/Reflex-DOM](https://reflex-frp.org/), web technologies like [HTML](https://developer.mozilla.org/en-US/docs/Web/HTML) and [CSS](https://developer.mozilla.org/en-US/docs/Web/CSS), and a terminal shell like [Bash](https://en.wikipedia.org/wiki/Bash_(Unix_shell)). Knowledge of [Nix](https://nixos.org/) helps but is not strictly necessary.


## Installing Obelisk

1. [Install Nix](https://nixos.org/nix/).
    If you already have Nix installed, make sure you have version 2.0 or higher.  To check your current version, run `nix-env --version`.
1. Set up nix caches
    1. If you are running NixOS, add this to `/etc/nixos/configuration.nix`:
        ```nix
        nix.binaryCaches = [ "https://nixcache.reflex-frp.org" ];
        nix.binaryCachePublicKeys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" ];
        ```
        and rebuild your NixOS configuration (e.g. `sudo nixos-rebuild switch`).
    1. If you are using another operating system or Linux distribution, ensure that these lines are present in your Nix configuration file (`/etc/nix/nix.conf` on most systems; [see full list](https://nixos.org/nix/manual/#sec-conf-file)):
        ```nix
        binary-caches = https://cache.nixos.org https://nixcache.reflex-frp.org
        binary-cache-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=
        binary-caches-parallel-connections = 40
        ```
        * If you're on a Linux distribution other than NixOS, enable sandboxing (see these [issue 172](https://github.com/obsidiansystems/obelisk/issues/172#issuecomment-411507818) or [issue 6](https://github.com/obsidiansystems/obelisk/issues/6) if you run into build problems) by adding the following:
          ```nix
          sandbox = true
          ```
          then restart the nix daemon
          ```bash
          sudo systemctl restart nix-daemon
          ```
        * If you're on MacOS, disable sandboxing (there are still some impure dependencies for now) by adding the following:
          ```nix
          sandbox = false
          ```
          then restart the nix daemon
          ```bash
          sudo launchctl stop org.nixos.nix-daemon
          sudo launchctl start org.nixos.nix-daemon
          ```
1. Install obelisk:
   ```bash
   nix-env -f https://github.com/obsidiansystems/obelisk/archive/master.tar.gz -iA command
   ```

### Accessing private repositories

To allow the Nix builder to access private git repositories, you must be set up
to access them via SSH. Follow these steps depending on the platform you need
access to:

- [GitHub](https://help.github.com/articles/connecting-to-github-with-ssh/)
- [GitLab](https://docs.gitlab.com/ee/gitlab-basics/create-your-ssh-keys.html)

## Developing an Obelisk project

To create a new Obelisk project, go to an empty directory and run:

```bash
ob init
```

Obelisk leverages ghcid to provide a live-reloading server that handles both frontend and backend. To run your Obelisk app and monitor the source for changes:

```bash
ob run
```

Now, with an appropriate browser, go to http://localhost:8000 (or the address/port specified in `config/common/route`) to access your app.
Firefox will not be able to properly run the development website due to [issue 460](https://github.com/obsidiansystems/obelisk/issues/460). Fortunately, this problem does not occur on a fully built website.

Every time you change the Haskell source files in frontend, common or backend, `ob run` will automatically recompile the modified files and reload the server. Furthermore, it will display on screen compilation errors and warnings if any.

### Local Hoogle

Obelisk can also provide a local [Hoogle](https://hoogle.haskell.org) server that lets you browse and search the types and documentation for all of the dependencies in your entire Obelisk application. To start the Hoogle server, in a spare terminal run the following command from the root of your Obelisk application:

```shell
$ ob hoogle
```

You can then access your local Hoogle from your web browser at `http://localhost:8080`, or by
instructing an editor plugin to use that address.

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

### Adding extra local packages

If the standard packages (`frontend`, `backend`, and `common`) are not
enough, to add more local Haskell packages, define them with the
`packages` parameter. The sources of these packages will be
automatically reloaded by `ob run`.

```nix
# ...
project ./. ({ pkgs, ... }: {
# ...
  packages = {
    another = ./another;
  };
# ...
```

### Running over HTTPS

To run your app locally over HTTPS, update the protocol in `config/common/route` to `https`, and then use `ob run` as normal.

Since Obelisk generates a self-signed certificate for running HTTPS, the browser will issue a warning about using an invalid certificate. On Chrome, you can go to `chrome://flags/#allow-insecure-localhost` to enable invalid certificates for localhost.

### IDE Support

Obelisk officially supports terminal-based feedback (akin to [`ghcid`](https://github.com/ndmitchell/ghcid)) in `ob run` and `ob watch`.

### Using GHC 8.10

Obelisk currently uses GHC 8.6 for projects by default, since this is the version on which Obelisk (and reflex-platform more generally) have been most thoroughly tested. However, we understand that this version is significantly behind GHC releases, and thus have experimental support for building with GHC 8.10 instead. To build with GHC 8.10, add the following to your project's `default.nix`:

```diff
  { system ? builtins.currentSystem
  , obelisk ? import ./.obelisk/impl {
      inherit system;
+     useGHC810 = true;
```

If the `useGHC810` argument is set to false, or not given, then GHC 8.6 will be used.

## Deploying

### Default EC2 Deployment

In this section we will demonstrate how to deploy your Obelisk app to an Amazon EC2 instance. Obelisk deployments are configured for EC2 by default (see [Custom Non-EC2 Deployment](#custom-non-ec2-deployment)).

Note: Most NixOS EC2 instances should just *work* regardless of obelisk version

First create a new EC2 instance:

1. Launch a NixOS 22.05 EC2 instance (we recommend [this AMI](https://us-east-1.console.aws.amazon.com/ec2/home?region=us-east-1#LaunchInstances:ami=ami-0223db08811f6fb2d))
1. In the instance configuration wizard ensure that your instance has at least 1GB RAM and 10GB disk space.
1. When prompted save your AWS private key (`~/myaws.pem`) somewhere safe. We'll need it later during deployment.
1. Go to "Security Groups", select your instance's security group and under "Inbound" tab add a new rule for HTTP port 80 and HTTPS port 443.

At this stage your instance should be booting and become accessible shortly. Note down the hostname of your EC2 instance.

Now go to your Obelisk project directory (`~/code/myapp`), and initialize a deployment config (`~/code/myapp-deploy`):
Your project directory must be "thunkable", i.e. something on which `ob thunk pack` can be called. Usually it will be a git repository whose current revision has been pushed upstream.

```bash
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

HTTPS is enabled by default; to disable HTTPS pass `--disable-https` to the `ob deploy init` command above.

This step will also require that you manually verify the authenticity of the host `$SERVER`.
You can specify that you want `ob deploy init` to check your `~/.ssh/known_hosts` file and save any fingerprints matching the host to the deployment-specific configuration by passing the `--check-known-hosts` option to the `deploy init` command.
Note that `--check-known-hosts` only works when there is a single keypair associated with a given host.


**REMARK (Security): Obelisk deployments do *not* rely on the `known_hosts` of your local machine during deployment, only potentially during the ob deploy init, as previously mentioned.**
This is because, in the event that you need to switch from one deploy machine / bastion host to another, you want to be absolutely sure that you're still connecting to the machines you think you are, even if that deploy machine / bastion host has never connected to them before.
Obelisk explicitly avoids a workflow that encourages people to accept host keys without checking them, since that could result in leaking production secrets to anyone who manages to MITM you, e.g. via DNS spoofing or cache poisoning.
Note that an active attack is a circumstance where you may need to quickly switch bastion hosts, e.g. because the attacker has taken one down or you have taken it down in case it was compromised.
In this circumstance you might need to deploy to production to fix an exploit or rotate keys, etc.
When you run `ob deploy` later it will rely on the saved verification in this step.

Next, go to the deployment directory that you just initialized and deploy!

```bash
cd ~/code/myapp-deploy
ob deploy push
```

`ob deploy push` will locally build your app and then transfer it, along with all the Nix package dependencies, via ssh to the EC2 instance. The backend will live in `/var/lib/backend`.

At this point you are done. Your app will be accessible at `${ROUTE}`. The currently deployed version - the git commit hash of the source repo - can be found at `${ROUTE}/version`.

### Custom Non-EC2 Deployment

By default Obelisk deployments are configured for NixOS machines running on AWS EC2. To provide your own configuration, you need to write a custom `module.nix` in the deployment repository. This still requires that your server is running NixOS.

`module.nix` must contain a Nix *function* that produces a [NixOS module function](https://nixos.org/nixos/manual/index.html#sec-writing-modules). The top-level function takes deployment configuration as arguments: `hostName`, `adminEmail`, `routeHost`, `enableHttps`, `version`, `exe`, `nixosPkgs`. Most of these are the values you specified during `ob deploy init` and are stored in the deployment repository. `version` is a `git` hash for the app that you're deploying. `exe` is the Linux build of the app (as seen in [Deploying Locally](#locally)). `nixosPkgs` is the package set used to construct the NixOS VM.

The [VirtualBox Deployment](#virtualbox-deployment) section provides an example.

#### VirtualBox Deployment

Here's a `module.nix` that is configured for deployment to a VirtualBox VM (running NixOS):

```nix
{ nixosPkgs, ... }: {...}: {
  imports = [ (nixosPkgs.path + /nixos/modules/virtualisation/virtualbox-image.nix) ];
}
```

The `{...}:` and following is the [NixOS module](https://nixos.org/nixos/manual/index.html#sec-writing-modules) definition.

### Locally

If you want deploy your application locally or test a production-oriented build you can build and deploy the app as described below.

Build the application:

```bash
nix-build -A exe --no-out-link
```

Copy the result to a new directory, add configuration, and run!

```bash
mkdir test-app
ln -s $(nix-build -A exe --no-out-link)/* test-app/
cp -r config test-app
(cd test-app && ./backend)
```

### From macOS

Deploying from macOS requires some extra setup:

- [Install nix-darwin](https://github.com/LnL7/nix-darwin)
- [Install docker](https://docs.docker.com/)

Running `ob deploy push` will give you additional setup instructions.

### Deploying an updated version

If you'd like to deploy an updated version (with new commits) of your Obelisk app: simply go to the configuration directory, update the source thunk and push:

```bash
cd ~/code/myapp-deploy
ob deploy update
ob deploy push
```

### Host Redirection

A `redirect_hosts` file can be added in the deployment directory (`~/code/myapp-deploy` in the example above), allowing you to specify alternative domain names that will redirect to the deployment domain.
This feature assumes the apropriate CNAME records have been added with a domain registration service.

Add one domain per line in `redirect_hosts`.
All listed domains will redirect to the publicly accessible domain specified by `ob deploy init`.
For clarity, this is the `$ROUTE` variable in the EC2 deployment example shown earlier.
The following is an example of a `~/code/myapp-deploy/redirect_hosts` file:

```
www.foo.com
www.bar.com
```

*Caveat*: Your https certificates will cover all your domains automatically, although you may need to force a recertification manually.
We assume you have root access to the deployment EC2 instance.
Continuing from the `ob init deploy` example above:

```bash
ssh root@ec2-35-183-22-197.ca-central-1.compute.amazonaws.com

EMAIL=myname@myapp.com
ROUTE_TO=myapp.com
ROUTE_FROM=foo.com
ROUTE_FROM_2=bar.com
/nix/store/`ls /nix/store | grep lego`/bin/lego \
  -d $ROUTE_TO \
  --email $EMAIL \
  --path . \
  --key-type ec256 \
  --accept-tos \
  -d $ROUTE_FROM \
  -d $ROUTE_FROM_2 \
  --http \
  --http.webroot /var/lib/acme/acme-challenge run
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
Install Xcode 11.2 (contains iOS SDK 13.2) and open it so that it runs its post install tool setup.

These versions will work out of the box but iOS SDKs prior to 11.3 should also work. You can choose another installed version in `default.nix`

More recent Xcodes should also work, as long as one of the SDKs mentioned above has been used.
To add another SDK to your current Xcode, [download](https://developer.apple.com/download/more/) the corresponding Xcode, extract it and copy its SDK folder next to the installed one, e.g.
```bash
open -W Xcode_9.2.xip
sudo cp -R Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS.sdk /Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS11.2.sdk
```


You can verify that you have correct versions by running
```bash
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
```bash
result-ios/bin/deploy [TEAM_ID]
# or in debug mode via lldb:
result-ios/bin/deploy [TEAM_ID] -d
```

#### Packaging
1. Go to [developer portal - distribution profiles](https://developer.apple.com/account/ios/profile/production).
Create and download a distribution profile.
1. Run the package script with your TEAM ID and your distribution profile to create a `.ipa`:
```bash
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


## Frequently Asked Questions (FAQ)

Refer to [FAQ](FAQ.md).


## Contributing

Contributions and issue reports are encouraged and appreciated! Refer to the [Contributing](CONTRIBUTING.md) guide for information about getting started.
