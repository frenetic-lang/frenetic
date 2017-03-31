# Symbolic NetKAT Automata
This directory contains NetKAT programs with interesting automata representations. (For more examples, consult the examples/global/ directory.)

## Requirements
You need graphviz:
  * MacOS: `brew install graphviz`
  * Ubuntu: `apt get install graphviz`

The frenetic executable must be installed:
  * run `make && make reinstall` in the frenetic root directory

## Run
To convert a NetKAT program into a symbolic NetKAT automaton, run
```bash
frenetic dump auto <file>
```
This will produce a .dot file named `foo.kat.auto.dot`, if `foo.kat` is the name of the input file.

You can then use graphviz to visualize the .dot file:
```bash
dot -Tsvg <.dot-file> -O  # create svg vector graphic
dot -Tpng <.dot-file> -O  # or, create png file
```

## Options
Run `frenetic dump help auto` to see a list of all options. Currently, two flags are supported:
  * --determinize: determinize the automaton using powerset construction
  * --minimize: (heuristically) minimize the automaton
By default, the automaton is neither minimized nor determinized.

