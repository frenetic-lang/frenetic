BENCH="../../_build/bench/src/Main.native"

all: run

build:
	BENCH=$(BENCH) bash build.sh

clean:
	rm -f *.json

run: build
	@echo "execute ./run.sh <json-file> to run benchmark"