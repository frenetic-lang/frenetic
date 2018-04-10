# Symbolic NetKAT Automata
This directory contains NetKAT programs with interesting automata representations. (For more examples, consult the examples/global/ directory.)

## Requirements
You need graphviz:
  * MacOS: `brew install graphviz`
  * Ubuntu: `apt get install graphviz`

The frenetic executable must be installed:
  * run `make && make install` in the frenetic root directory

## Run
To convert a NetKAT program into a symbolic NetKAT automaton, run
```bash
frenetic dump auto <file> --render-auto
```
The flag `--render-auto` is optional; if provided, `frenetic` will automatically invoke graphviz (provided `dot` is on the PATH) and open the resulting file in its associated default application.

In any case, the command will produce a .dot file named `foo.kat.auto.dot`, if `foo.kat` is the name of the input file.

You can then invoke graphviz manually to visualize the .dot file:
```bash
dot -Tsvg <.dot-file> -O  # create svg vector graphic
dot -Tpng <.dot-file> -O  # or, create png file
```

## Options
Run `frenetic dump help auto` to see a list of all options. Currently, three flags are supported:
* --render-auto: this will automatically invoke graphviz (provided `dot` is on the PATH) and open the resulting file with the default associated application
* --determinize: determinize the automaton using powerset construction
* --minimize: (heuristically) minimize the automaton
* --remove-topo: remove topology states from the automaton. Not equivalence preserving!

By default, the automaton is neither minimized nor determinized.

