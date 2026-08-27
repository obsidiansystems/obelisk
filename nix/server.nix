# NixOS module for deploying an obelisk backend application.
#
# Usage: import this module in your NixOS configuration and set
# services.obelisk options. Use obelisk's serverExe to build the exe.
{ config, lib, pkgs, ... }:

let cfg = config.services.obelisk;

in {
  options.services.obelisk = {
    enable = lib.mkEnableOption "obelisk backend application";

    exe = lib.mkOption {
      type = lib.types.package;
      description = "Deployment directory containing the backend executable and assets.";
    };

    name = lib.mkOption {
      type = lib.types.str;
      default = "backend";
      description = "Service name.";
    };

    user = lib.mkOption {
      type = lib.types.str;
      default = cfg.name;
      description = "User to run the backend as.";
    };

    group = lib.mkOption {
      type = lib.types.str;
      default = cfg.user;
      description = "Group for the backend user.";
    };

    routeHost = lib.mkOption {
      type = lib.types.str;
      description = "Domain name for the main virtual host.";
    };

    enableHttps = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Whether to enable HTTPS via Let's Encrypt.";
    };

    adminEmail = lib.mkOption {
      type = lib.types.str;
      default = "";
      description = "Email for ACME certificate registration. Required when enableHttps is true.";
    };

    acmeAcceptTerms = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Whether to accept the ACME terms of service.";
    };

    internalPort = lib.mkOption {
      type = lib.types.port;
      default = 8000;
      description = "Port the backend listens on internally.";
    };

    backendArgs = lib.mkOption {
      type = lib.types.str;
      default = "--port=${toString cfg.internalPort}";
      description = "Arguments passed to the backend executable.";
    };

    baseUrl = lib.mkOption {
      type = lib.types.str;
      default = "/";
      description = "Base URL path for the nginx proxy location.";
    };

    redirectHosts = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [];
      description = "Domains that redirect to routeHost. Also added to the SSL certificate.";
    };

    configHash = lib.mkOption {
      type = lib.types.str;
      default = "";
      description = "Change this to force a service restart when config files change.";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [{
      assertion = !(builtins.elem cfg.routeHost cfg.redirectHosts);
      message = "services.obelisk: routeHost may not be a member of redirectHosts";
    }];

    networking.firewall.allowedTCPPorts =
      if cfg.enableHttps then [ 80 443 ] else [ 80 ];

    services.openssh = {
      enable = lib.mkDefault true;
      settings.PermitRootLogin = lib.mkDefault "prohibit-password";
    };

    security.acme = lib.mkIf cfg.enableHttps {
      acceptTerms = cfg.acmeAcceptTerms;
      defaults.email = cfg.adminEmail;
      certs."${cfg.routeHost}".extraDomainNames = cfg.redirectHosts;
    };

    services.nginx = {
      enable = true;
      recommendedProxySettings = true;
      virtualHosts = {
        "${cfg.routeHost}" = {
          enableACME = cfg.enableHttps;
          forceSSL = cfg.enableHttps;
          locations.${cfg.baseUrl} = {
            proxyPass = "http://127.0.0.1:${toString cfg.internalPort}";
            proxyWebsockets = true;
            extraConfig = "access_log off;";
          };
        };
      } // builtins.listToAttrs (map (host: {
        name = host;
        value = {
          enableACME = cfg.enableHttps;
          forceSSL = cfg.enableHttps;
          globalRedirect = cfg.routeHost;
        };
      }) cfg.redirectHosts);
    };

    systemd.services.${cfg.name} = {
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      restartIfChanged = true;
      path = [ pkgs.gnutar ];
      script = ''
        echo "Config hash: ${cfg.configHash}"
        ln -sft . '${cfg.exe}'/*
        mkdir -p log
        exec ./backend ${cfg.backendArgs} </dev/null
      '';
      serviceConfig = {
        User = cfg.user;
        KillMode = "process";
        WorkingDirectory = "~";
        Restart = "always";
        RestartSec = 5;
      };
    };

    users.users.${cfg.user} = {
      description = "${cfg.user} service";
      home = "/var/lib/${cfg.user}";
      createHome = true;
      isSystemUser = true;
      group = cfg.group;
    };

    users.groups.${cfg.group} = {};
  };
}
