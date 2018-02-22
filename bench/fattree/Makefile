BENCH="../../_build/default/bench/src/benchmark.exe"

all: run

build:
	BENCH=$(BENCH) bash build.sh

clean:
	rm -f *.json

run: build
	@echo "execute ./run.sh <json-file> to run benchmark"
