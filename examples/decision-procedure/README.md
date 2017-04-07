# Decision Procedure
This directory contains (pairs of) NetKAT programs that can be checked for equivalence.

## Requirements
The frenetic executable must be installed:
  * run `make && make reinstall` in the frenetic root directory

## Run
To decide program equivalence, run
```bash
frenetic dump decision <file1> <file2>
```

## Options
Run `frenetic dump help decision` to see a list of all options. Currently, one flag is supported:
  * --dump-auto: dump automata representations of the programs

