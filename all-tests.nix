{
  supportedSystems ? [ builtins.currentSystem ]
}:
let
  nginxRoot = "/run/nginx";
  obelisk = import ./default.nix {};
  # Get NixOS a pre-release 20.03 that contains the python based tests and recursive nix
  pkgs = import (builtins.fetchTarball https://github.com/nixos/nixpkgs/archive/3de5266.tar.gz) {};
  sshKeys = import (pkgs.path + /nixos/tests/ssh-keys.nix) pkgs;
  make-test = import (pkgs.path + /nixos/tests/make-test-python.nix);
  obelisk-everywhere = (import ./all-builds.nix { inherit supportedSystems; }).x86_64-linux.cache;
  snakeOilPrivateKey = sshKeys.snakeOilPrivateKey.text;
  snakeOilPublicKey = sshKeys.snakeOilPublicKey;
in
  make-test ({...}: {
    name  = "obelisk";
    nodes = {
      githost = {
        networking.firewall.allowedTCPPorts = [ 22 80 ];
        services.openssh = {
          enable = true;
        };
        environment.systemPackages = [
          pkgs.git
        ];
        users.users.root.openssh.authorizedKeys.keys = [
          snakeOilPublicKey
        ];
      };

      client = {
        imports = [
          (pkgs.path + /nixos/modules/installer/cd-dvd/channel.nix)
        ];
        nix.useSandbox = false;
        nix.binaryCaches = [];
        environment.systemPackages = [
          obelisk.command
          obelisk.shell
          obelisk-everywhere
          pkgs.git
        ];
      };
    };

    testScript =
      let
        privateKeyFile = pkgs.writeText "id_rsa" ''${snakeOilPrivateKey}'';
        thunkableSample = pkgs.writeText "default.nix" ''
          let pkgs = import <nixpkgs> {}; in pkgs.git
        '';
        invalidThunkableSample = pkgs.writeText "default.nix" ''
          let pkgs = import <nixpkgs> {}; in pkgtypo.git
        '';
        sshConfigFile = pkgs.writeText "ssh_config" ''
          Host *
            StrictHostKeyChecking no
            UserKnownHostsFile=/dev/null
            ConnectionAttempts=1
            ConnectTimeout=1
            IdentityFile=~/.ssh/id_rsa
            User=root
        '';
      in ''
      start_all()

      with subtest("obelisk is installed and git is configured"):
          client.succeed("ob --help")
          client.succeed('git config --global user.email "you@example.com"')
          client.succeed('git config --global user.name "Your Name"')

      githost.wait_for_open_port("22")

      with subtest("the client can access the server via ssh"):
          client.succeed("mkdir -p ~/.ssh/")
          client.succeed(
              "cp ${privateKeyFile} ~/.ssh/id_rsa"
          )
          client.succeed("chmod 600 ~/.ssh/id_rsa")
          client.wait_until_succeeds(
              "ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no -i ~/.ssh/id_rsa githost true"
          )
          client.succeed(
              "cp ${sshConfigFile} ~/.ssh/config"
          )
          client.wait_until_succeeds("ssh githost true")

      with subtest("a remote bare repo can be started"):
          githost.succeed("mkdir -p ~/myorg/myapp.git")
          githost.succeed("cd ~/myorg/myapp.git && git init --bare")

      with subtest("a git project can be configured with a remote using ssh"):
          client.succeed("mkdir -p ~/code/myapp")
          client.succeed("cd ~/code/myapp && git init")
          client.succeed(
              "cp ${thunkableSample} ~/code/myapp/default.nix"
          )
          client.succeed("cd ~/code/myapp && git add .")

          client.succeed('cd ~/code/myapp && git commit -m "Initial"')
          client.succeed(
              "cd ~/code/myapp && git remote add origin root@githost:/root/myorg/myapp.git"
          )


      with subtest("pushing code to the remote"):
          client.succeed("cd ~/code/myapp && git push -u origin master")
          client.succeed("cd ~/code/myapp && git status")

      with subtest("obelisk can pack"):
          client.succeed("ob -v thunk pack ~/code/myapp")
          client.succeed("grep -qF 'git.json' ~/code/myapp/thunk.nix")
          client.succeed("grep -qF 'myorg' ~/code/myapp/git.json")
          client.succeed("ob -v thunk unpack ~/code/myapp")

      with subtest("obelisk can set the public / private flag"):
          client.succeed("ob -v thunk pack ~/code/myapp --private")
          client.fail("""grep -qF '"private": practice' ~/code/myapp/git.json""")
          client.succeed("""grep -qF '"private": true' ~/code/myapp/git.json""")
          client.succeed("nix-build ~/code/myapp")
          client.succeed("ob -v thunk unpack ~/code/myapp")
          client.succeed("ob -v thunk pack ~/code/myapp --public")
          client.succeed("""grep -qF '"private": false' ~/code/myapp/git.json""")
          client.succeed("nix-build ~/code/myapp")
          client.succeed("ob -v thunk unpack ~/code/myapp")

      with subtest("building an invalid thunk fails"):
          client.succeed("cd ~/code/myapp/local && git checkout -b bad")
          client.succeed(
              "cp ${invalidThunkableSample} ~/code/myapp/local/default.nix"
          )
          client.succeed("cd ~/code/myapp/local && git add .")
          client.succeed('cd ~/code/myapp/local && git commit -m "Bad commit"')
          client.succeed("cd ~/code/myapp/local && git push -u origin bad")
          client.succeed("ob -v thunk pack ~/code/myapp --public")
          client.fail("nix-build ~/code/myapp")
          client.succeed("ob -v thunk unpack ~/code/myapp")
          client.succeed("cd ~/code/myapp/local && git checkout master")

      with subtest("obelisk can detect private repos"):
          client.succeed("ob -v thunk pack ~/code/myapp")
          client.succeed("""grep -qF '"private": true' ~/code/myapp/git.json""")
          client.succeed("ob -v thunk unpack ~/code/myapp")
      '';
  }) {}
