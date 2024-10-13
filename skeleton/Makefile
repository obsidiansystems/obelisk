root = $(shell pwd)
repl = cabal repl dev all

define ghcid
  ghcid                                       \
    -c '$(repl)'                              \
    --warnings                                \
    --restart 'common/common.cabal'           \
    --restart 'backend/backend.cabal'         \
    --restart 'frontend/frontend.cabal'       \
    --restart 'dev/dev.cabal'
endef

# Call inside `nix-shell`
all: build

hoogle:
	hoogle server --local -p 8080

build:
	cabal build all

repl:
	$(repl)

watch:
	$(ghcid)

.PHONY: dev
dev:
	$(ghcid)                           \
	--restart 'config'                 \
	--test 'Dev.dev "$(root)"'
