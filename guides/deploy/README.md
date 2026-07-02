# Deploying an Obelisk app to a NixOS server

This walkthrough takes a working Obelisk project from `nix-build` to a NixOS
host serving it over HTTPS, and covers updating it afterwards. It replaces
the v1 `ob deploy` workflow (see
[docs/migrating-to-v2.md](../../docs/migrating-to-v2.md) for the mapping).

Prerequisites:

- a NixOS host you can SSH into as root (any cloud image or your own
  hardware; NixOS is the only assumption),
- a DNS record for your domain pointing at the host,
- a workstation with Nix set up per [docs/setup.md](../../docs/setup.md).

## 1. Build the server executable

From your project root:

```bash
nix-build . -A serverExe.wasm    # or serverExe.js for the GHCJS frontend
```

The result is a directory: the `backend` binary plus the compressed frontend
and static assets, and the project's bundled public config. This directory is
what the NixOS module runs.

## 2. Write the host configuration

Create a NixOS configuration for the host that imports obelisk's server
module. For a flake-based host config:

```nix
# flake.nix on your ops repository (or the app repository itself)
{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
  outputs = { self, nixpkgs }: {
    nixosConfigurations.myhost = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ./hardware-configuration.nix   # from the host's /etc/nixos/
        ./myapp.nix
      ];
    };
  };
}
```

```nix
# myapp.nix
{ config, ... }:
let app = import ./path/to/my-app { system = "x86_64-linux"; };
in {
  imports = [ app.serverModule ];

  services.obelisk = {
    enable = true;
    exe = app.serverExe.wasm;
    routeHost = "myapp.example.com";
    enableHttps = true;
    adminEmail = "admin@example.com";
    acmeAcceptTerms = true;   # Let's Encrypt terms of service
  };
}
```

The module (`nix/server.nix` in obelisk) sets up:

- nginx as a reverse proxy on ports 80/443 with WebSocket support,
- an ACME (Let's Encrypt) certificate for `routeHost`, renewed automatically,
- a systemd service that runs the backend as its own system user from
  `/var/lib/<name>`, restarting on failure,
- firewall rules for 80/443 and SSH with password login disabled for root.

Other options: `redirectHosts` (extra domains 301-redirecting to
`routeHost`), `internalPort` (the loopback port nginx proxies to, default
8000), `backendArgs`, `baseUrl`, and `name`/`user`/`group`. See
`nix/server.nix` for the full list.

## 3. Deploy

Two equivalent ways; both build the system closure locally, copy it to the
host over SSH, and activate it.

With obelisk's helper (available in the dev shell):

```bash
ob-deploy .#nixosConfigurations.myhost root@myhost
```

Or with plain nixos-rebuild:

```bash
nixos-rebuild switch --flake .#myhost --target-host root@myhost
```

`ob-deploy` always activates with `switch`; if the deploy changed the kernel
or initrd, reboot the host yourself.

## 4. Verify

```bash
curl -sI https://myapp.example.com/ | head -3
```

Expect `HTTP/2 200` and the server-rendered page at `/`. On the host,
`journalctl -u <name>` (default `backend`) shows the backend log, and the
service files live in `/var/lib/backend`.

## 5. Update

Updating is redeploying: commit your changes, then re-run step 3. The
closure copy only transfers what changed, nginx keeps serving during the
switch, and the backend restarts on the new binary. Roll back with
`nixos-rebuild switch --rollback --target-host root@myhost` (or select the
previous generation from the boot menu).

## Backend secrets

The bundled config in `serverExe` contains only the public `common/` and
`frontend/` parts (that is `obelisk.config.path`'s contract: `backend/`
secrets never enter the Nix store). The `services.obelisk` service symlinks
the exe's read-only bundled config into place, so it has no hook for adding
secret files. If your backend reads `config/backend/*`:

- use [obelisk-systemd](https://github.com/obsidiansystems/obelisk-systemd)
  instead of `services.obelisk`; its `configSource` option points at a
  host-side directory holding the full runtime config, secrets included
  (see the README's Deployment section), or
- leave `obelisk.config.path` unset in `project.nix` and manage the whole
  `config/` directory in the service's working directory
  (`/var/lib/<name>/config`) yourself.

## Alternative: OCI container

If the host runs containers instead of NixOS services:

```bash
nix-build . -A containerImage.wasm
podman load < result
podman run -p 8000:8000 my-app:latest
```

The image runs the same server directory with `--port=8000`; put your own
TLS-terminating proxy in front.
