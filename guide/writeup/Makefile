.PHONY=all clean

all: 
	latexmk -pdf main

long:
	pdflatex main; bibtex main; pdflatex main; pdflatex main

quick:
	pdflatex main

clean:
	latexmk -c main
