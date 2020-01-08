let 
  nginxRoot = "/run/nginx";
  obelisk = import ./default.nix {};
  # Get NixOS a pre-release 20.03 that contains the python based tests and recursive nix
  pkgs = import (builtins.fetchTarball https://github.com/nixos/nixpkgs/archive/3de5266.tar.gz) {};
  supportedPlatforms = [ "x86_64-linux" "x86_64-darwin" ];
  sshKeys = import (pkgs.path + /nixos/tests/ssh-keys.nix) pkgs;
  make-test = import (pkgs.path + /nixos/tests/make-test-python.nix);
  #supportedPlatforms = [ "x86_64-linux" ];
  obelisk-everywhere = (import ./everywhere.nix { cacheBuildSystems = supportedPlatforms; }).metaCache;
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
          pkgs.jq
        ];
      };
    };

    testScript =
      let
        privateKeyFile = pkgs.writeText "id_rsa" ''${snakeOilPrivateKey}'';
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
      githost.wait_for_open_port("22")
      
      with subtest("test obelisk is installed"):
          client.succeed("ob --help")
      
      with subtest("test the client can access the server via ssh"):
          client.succeed("mkdir -p ~/.ssh/")
          client.succeed(
              "cp ${privateKeyFile}  ~/.ssh/id_rsa"
          )
          client.succeed("chmod 600 ~/.ssh/id_rsa")
          client.wait_until_succeeds(
              "ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no -i ~/.ssh/id_rsa githost true"
          )
          client.succeed(
              "cp ${sshConfigFile} ~/.ssh/config"
          )
          client.wait_until_succeeds("ssh githost true")
      
      with subtest("test a remote bare repo can be started"):
          githost.succeed("mkdir -p ~/myorg/myapp.git")
          githost.succeed("cd ~/myorg/myapp.git && git init --bare")
      
      with subtest("test a git project can be configured with a remote using ssh"):
          client.succeed("mkdir -p ~/code/myapp")
          client.succeed("cd ~/code/myapp && git init")
          client.succeed("cd ~/code/myapp && touch README")
          client.succeed("cd ~/code/myapp && git add .")
          client.succeed('git config --global user.email "you@example.com"')
          client.succeed('git config --global user.name "Your Name"')
          client.succeed('cd ~/code/myapp && git commit -m "Initial"')
          client.succeed(
              "cd ~/code/myapp && git remote add origin root@githost:/root/myorg/myapp.git"
          )
      
      with subtest("test pushing code to the remote"):
          client.succeed("cd ~/code/myapp && git push -u origin master")
          client.succeed("cd ~/code/myapp && git status")
      
      with subtest("test obelisk can pack"):
          client.succeed("ob thunk pack ~/code/myapp")
          client.succeed("grep -qF 'git' ~/code/myapp/default.nix")
          client.succeed("grep -qF 'myorg' ~/code/myapp/git.json")
      
      with subtest("test obelisk can detect private repos"):
          client.succeed("""grep -qF '"private": false' ~/code/myapp/git.json""")
    '';
  })
