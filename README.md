Assuming you've already added SSH keys to both your github and gitlab
accounts, please proceed with the instructions that follow. 

In your new project's git repository:

```bash
git init
```
The ob-init.hs script will add and update the obelisk submodule as well as
generate the necessary boilerplate files and folders. Enter the following in
your console, followed by your project's name when prompted. 

```bash
./ob-init.hs
```
Build the frontend by running ./obelisk/build-frontend

Now you can try running the backend in GHCi by running

```bash
./obelisk/ghci-backend
```
and then typing

```bash
>main
```

at the GHCi prompt.

Point your web browser at [localhost:8000](localhost:8000) and everything should work.

Feel free to edit the frontend and backend directories as you see fit.

--------------------------DEVELOPER TOOLS----------------------------------------
To increase developer productivity it is highly recommended to make use of the 
`ghcid-frontend` and `ghcid-backend` scripts. This will open a live repl that
will compile and refresh itself showing you the latest possible syntax or type errors 
as well as any other ghc warnings whenever files within their respective directories
are saved and updated.

Thank you for using Obelisk. 
