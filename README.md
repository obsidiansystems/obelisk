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
    1. `nix-env -i hub`
    1. `hub clone obsidiansystems/obelisk`
      * NOTE: you must authenticate with hub at least once, because the `ob` command uses `hub` for authentication
      #TODO: Make ob do this itself (either invoke hub automatically or not depend on hub)
1. Install `ob`: `nix-env -f obelisk -iA command`

## Building
Build the frontend by running

```bash
nix-build -A ghcjs.frontend --out-link frontend-js
```

Now you can try running the backend in GHCi by running

```bash
nix-shell -A ghc.backend.env --run "cd backend; cabal repl"
```
and then typing

```bash
main
```

at the GHCi prompt.

Point your web browser at [localhost:8000](localhost:8000) and everything should work.

Feel free to edit the frontend and backend directories as you see fit.

--------------------------DEVELOPER TOOLS----------------------------------------

To increase developer productivity it is highly recommended to make use of the
`ob watch` command. This will open a live repl that
will compile and refresh itself showing you the latest possible syntax or type errors
as well as any other ghc warnings whenever files within their respective directories
are saved and updated.

-----------------------DEPLOYMENT------------------------------------------------

From Nix to NixOS:
When you feel your repository is ready to be deployed, feel free to utilize some scripts to aid in the process.

Assuming you have taken the neccessary steps in setting up your staging/production server: #TODO: Elaborate on this setup as well.

From within the root of your obelisk project directory, use the script and syntax provided below:

```bash
./set-deploy [DESTINATION DIR] [PRIVATE KEY FILE]
```

This script will create a `deploy` directory within the directory you've specified within the first arguement to the script. To start deployment to your NixOS server, `cd` into the `deploy/src` directory and use the following script and syntax provided below:

```bash
./obelisk/deploy [yourServer.org] .. --arg ssl true
```
