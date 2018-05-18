## Installation
1. Set up nix caches
    1. Add this to `/etc/nixos/configuration.nix`:
        ```
        nix.binaryCaches = [ "https://nixcache.reflex-frp.org" ];
        nix.binaryCachePublicKeys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" ];
        ```
1. Get set up to access private repositories
    1. [Get set up to connect to GitHub with SSH](https://help.github.com/articles/connecting-to-github-with-ssh/)
    1. [Create a GitHub personal access token](https://help.github.com/articles/creating-a-personal-access-token-for-the-command-line/)
    1. Add this to `/etc/nixos/configuration.nix`:
        ```
        nix.envVars = {
          NIX_GITHUB_PRIVATE_USERNAME = "your-github-username";
          NIX_GITHUB_PRIVATE_PASSWORD = "your-github-personal-access-token";
        };
        ```
    1. `nix-env -i hub` OR `nix-env -iA nixos.gitAndTools.hub`
    1. `hub clone obsidiansystems/obelisk`
      * NOTE: you must authenticate with hub at least once, because the `ob` command uses `hub` for authentication
      #TODO: Make ob do this itself (either invoke hub automatically or not depend on hub)
1. Install `ob`: `nix-env -f obelisk -iA command`.
   Alternatively, if you prefer not to install to your user nix environment, you can
   enter a shell with the `ob` command available: `nix-shell -A shell`.
   After running `ob init` this becomes `nix-shell .obelisk/impl -A shell`
   for subsequent shells (since `ob init` overwrites `default.nix`).

## Building
Build the frontend by running

```bash
nix-build -A ghcjs.frontend --out-link frontend-js
```

Now you can try running the backend in GHCi by running

```bash
ob repl backend
```
and then typing

```bash
backend
```

at the GHCi prompt.

Point your web browser at [localhost:8000](localhost:8000) and everything should work.

Feel free to edit the frontend and backend directories as you see fit.

## Developer Tools
To increase developer productivity it is highly recommended to make use of the
`ob watch` command. This will open a live repl that
will compile and refresh itself showing you the latest possible syntax or type errors
as well as any other ghc warnings whenever files within their respective directories
are saved and updated.

## Deploying

In this section we will demonstrate how to deploy your Obelisk app to an Amazon EC2 instance.

First create a new EC2 instance:

1. Launch a NixOS 17.09 EC2 instance (we recommend [this AMI](https://console.aws.amazon.com/ec2/v2/home?region=us-east-1#LaunchInstanceWizard:ami=ami-40bee63a)) 
1. In the instance configuration wizard ensure that your instance has at least 1GB RAM and 10GB disk space.
1. When prompted save your AWS private key (`~/myaws.pem`) somewhere safe. We'll need it later during deployment.
1. Go to "Security Groups", select your instance's security group and under "Inbound" tab add a new rule for HTTP port 80.

At this stage your instance should be booting and become accessible shortly. Note down the hostname of your instance. It should look like this:

```
INSTANCE_HOSTNAME=ec2-??-??-??-??.ca-central-1.compute.amazonaws.com
```

Now go to your Obelisk project directory (`~/code/myapp`), and initialize a deployment config (`~/code/myapp-deploy`):

```
cd ~/code/myapp
ob deploy init --ssh-key ~/myaws.pem --hostname ${INSTANCE_HOSTNAME} ~/code/myapp-deploy
```

Then go to that created deployment configuration directory, and initiate the deployment:

```
cd ~/code/myapp-deploy
ob deploy push
```

`ob deploy push` will locally build your app and then transfer it, along with all the Nix package dependencies, via ssh to the EC2 instance. It will also configure Nginx so that the public port 80 proxies to the running app.

At this point you are done. Your app will be accessible at `http://${HOSTNAME}`!

### Deploying an updated version

If you'd like to deploy an updated version (with new commits) of your Obelisk app: simply go to the configuration directory, update the source thunk and push:

```
cd ~/code/myapp-deploy
ob deploy update
ob deploy push
```

## Building for mobile

### Android 

Until Obelisk will be able to automate this workflow via single command you are recommended to build android apps manually as follows.

1. In your project's `default.nix` set a suitable value for `android.applicationId` and `android.displayName`.
1. Run `nix-build -A android.frontend -o result-android` to build the Android app.
1. A debug version of the app should be generated at `result-android/android-app-debug.apk`

Now deploy the built apk file to your Android device:

1. Enable *USB debugging* in your Android device
1. Connect the device using USB
1. Run the deploy script: `result-android/bin/deploy`

This should copy over and install the application on your device (if you see a  "*signatures do not match* error, simply uninstall the previous app from the device before retrying the deploy). The name of the installed application will be what you have specified for `android.displayName` in the `default.nix`.

#### Releasing to Play Store

##### Configure signing

The previous section would have generated a debug version of the app. In order to build a release version you will need to sign your app. Obelisk can automatically sign the app during build if you provide it with your keystore file in `default.nix`.

First, if you do not already have a keystore, create it as follows (for more information, see the [Android documentation](https://developer.android.com/studio/publish/app-signing#signing-manually):

```
nix-shell -p androidenv.platformTools --run "keytool -genkey -v -keystore myandroidkey.jks -keyalg RSA -keysize 2048 -validity 10000 -alias myandroidalias
```

(Besure to give an appropriate keystore filename and alias string above.)

The `keytool` command will prompt for some details, including a keystore password and a key password (we will use these passwords further below). It will now have created a `myandroidkey.jks` file under the current directory. Move that to somewhere safe, and note down its full path.

Now edit your project's `default.nix` and tell Obelisk of your app's keystore file. Your `default.nix` should look like this after the edit:

```nix
  android.releaseKey = 
    { storeFile = /path/to/myandroidkey.jks;  
      storePassword = "abcd1234";
      keyAlias = "myandroidalias";
      keyPassword = "abcd1234";
    };
```

##### Build a release version

After having configured signing for your app, you may proceed to build a release version of the app. This is no different to how you build it, so consult the section [Android](#Android) for exact instructions on building and deploying to your device.



