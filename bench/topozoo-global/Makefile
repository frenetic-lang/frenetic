TOPOS=$(shell cat dot-files)
POLS=$(TOPOS:.dot=.json)
COMPILEKAT="../../_build/bench/src/Main.native"

all: run

%.dot:
	wget http://storage.googleapis.com/compilekat/dots/topozoo/$@

%.json: %.dot
	$(COMPILEKAT) global-routing $^ $@

download: $(POLS)

clean:
	rm -f *.json *.dot log.txt

run: download
	bash run_all.sh