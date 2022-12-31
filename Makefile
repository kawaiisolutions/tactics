ifdef DEBUG
SPINFLAGS = WASMTIME_BACKTRACE_DETAILS=1
SPINFLAGS += RUST_LOG=spin=trace
endif

ifndef SPINCFG
SPINCFG = spin.toml
endif

.PHONY: run watch container deploy

all: run

build:
	spin build --file $(SPINCFG)

run: build
	$(SPINFLAGS) spin up --file $(SPINCFG)

watch: build
	nodemon --watch cgi-bin --watch . --ext pl,html --verbose --signal SIGINT --exec '$(SPINFLAGS) spin up --file $(SPINCFG)'
