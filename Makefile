ifdef DEBUG
SPINFLAGS = WASMTIME_BACKTRACE_DETAILS=1
SPINFLAGS += RUST_LOG=spin=trace
endif

ifndef SPINCFG
SPINCFG = spin.toml
endif

.PHONY: run watch

all: run

build:

run: # build
	$(SPINFLAGS) spin up --file $(SPINCFG)

watch: # build
	$(SPINFLAGS) spin watch --skip-build --direct-mounts --file $(SPINCFG)
