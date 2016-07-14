BENCH="../../_build/bench/src/Main.native"

all: run

build:
	BENCH=$(BENCH) bash build.sh
	rm -f seeds/*.bench

clean:
	rm -f seeds/*.bench *.json

run: build
	@echo "execute ./run.sh <json-file> to run benchmark"