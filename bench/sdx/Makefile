POLS=$(shell cat pol-files)
COMPILEKAT="../../_build/bench/src/Main.native"

all: run

%.json:
	wget http://storage.googleapis.com/compilekat/pols/sdx-netkat/$@

download: $(POLS)

clean:
	rm -f *.json

run: download
	@echo "execute ./run.sh <json-file> to run benchmark"