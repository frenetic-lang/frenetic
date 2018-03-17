TOPOS=$(shell cat dot-files)
POLS=$(TOPOS:.dot=.json)
COMPILEKAT="../../_build/default/bench/src/benchmark.exe"

all: run

%.dot:
        wget https://s3.amazonaws.com/compilekat/dots/topozoo/$@


%.json: %.dot
	$(COMPILEKAT) global-routing $^ $@

download: $(POLS)

clean:
	rm -f *.json *.dot log.txt

run: download
	bash run_all.sh