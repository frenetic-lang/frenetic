TOPOS=$(shell cat dot-files)
POLS=$(TOPOS:.dot=.json)
COMPILEKAT="../../_build/bench/src/Main.native"

all: run

%.dot:
	wget https://s3.amazonaws.com/compilekat/dots/topozoo/$@

%.json: %.dot
	$(COMPILEKAT) dot-to-json $^ $@

download: $(POLS)

clean:
	rm -f *.json *.dot log.txt

run: download
	bash run_all.sh
