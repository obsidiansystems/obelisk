### Config

An Obelisk project may contain a `config/` folder with `common/`, `frontend/`,
and `backend/` subfolders. Config is **optional** — the framework runs fine
without it — but the skeleton ships a starter `config/common/route` so new
projects have the batteries in place.

- `common/` — configuration shared by frontend and backend (e.g. `route`, the
  app's canonical root URL). These are *public*: the backend injects them into
  the served page and the frontend can read them at runtime.
- `frontend/` — frontend-only public configuration.
- `backend/` — server-only configuration. Things that must never reach the
  frontend (e.g. email or database credentials) belong here.

Read a value anywhere you have `HasConfigs` (every frontend widget, and the
backend) with `getTextConfig` from `Obelisk.Configs`:

    route <- maybe "" T.strip <$> getTextConfig "common/route"

#### `common/route`

Holds the app's canonical root URL (`http://localhost:8000` in development).
Unlike Obelisk v1 this is **no longer required** — routing is relative
(`<base href="/">`) and the route encoder is host-independent. Keep it for code
that needs to build *absolute* URLs (canonical/OpenGraph tags, share links,
OAuth redirects):

    base <- maybe "" T.strip <$> getTextConfig "common/route"
    pure (base <> renderFrontendRoute checkedFullRouteEncoder someRoute)

#### Production

The nix build bundles only the **public** config (`common/` and `frontend/`)
into the server exe. `backend/` is intentionally *not* baked into the build, so
secrets never enter the Nix store — supply backend secrets to the running
server at runtime. The directory to bundle is set by `obelisk.config.path` in
`project.nix` (the skeleton points it at `./config`).
