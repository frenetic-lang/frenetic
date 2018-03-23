# Welcome to Frenetic

[![Build Status](https://travis-ci.org/frenetic-lang/frenetic.svg?branch=master)](https://travis-ci.org/frenetic-lang/frenetic)

The languages used to program networks today lack modern features. Programming them is a complicated and error-prone task, and outages and infiltrations are frequent. Frenetic is an open-source Software Defined Network (SDN) controller platform designed to make SDN programming easy, modular, and semantically correct, based on _programming languages_ with the following essential features:

* _High-level abstractions_ that give programmers direct control over the network, allowing them to specify what they want the network to do without worrying about how to implement it.
* _Modular constructs_ that facilitate compositional reasoning about programs.
* _Portability_, allowing programs written for one platform to be reused with different devices.
* _Rigorous semantic foundations_ that precisely document the meaning of the language and provide a solid platform for mechanical program analysis tools.

You can build Frenetic-based network applications with:

* OCaml
* Python
* REST and JSON (i.e., any programming language!)

## Getting Started

### Installation

1. Install [OPAM](https://opam.ocaml.org/)

1. Switch to OCaml version 4.06.0 or greater:
    ```
    opam switch 4.06.0    
    ```        

1. Install required OCaml dependencies. Note that JBuilder can compute the list of dependencies, 
    ```
    jbuilder external-lib-deps --missing @install
    ```
    and you can install each using OPAM---for example:
    ```
    opam install ocamlgraph    
    ```
    
    1. Build Frenetic
    ```
    make && make install
    ```
    
1. (Optional) install [Mininet](http://mininet.org/)
        
### Hello World in SDN

The following instructions assume a Linux host with Mininet installed. 
    
1.  Start up a terminal window.

2.  Start up a Mininet sample network with a switch and 2 hosts:

    ```
    $ sudo mn --topo=single,2 --controller=remote
    ```

3.  Try pinging the host h2 from the host h1:

    ```
    mininet> h1 ping h2
    ```

    Unfortunately, the ping won't work because you don't have an SDN network program in place!  Press CTRL-C to stop the pinging.
4.  Start up another terminal window and start up Frenetic:

    ```
    $ frenetic http-controller --verbosity debug
    ```
5.  In a third terminal window, start up the example program for the Python repeater:

    ```
    $ python -m frenetic.examples.repeater
    ```
6.  Now, back in the window running Mininet, the ping should now succeed:

    ```
    mininet> h1 ping h2
    ```

    Congratulations!  You now have a working Software Defined Network.

### Where to Go From Here

* Learn Frenetic programming in Python from the [Frenetic Programmers Guide](https://github.com/frenetic-lang/manual/blob/master/programmers_guide/frenetic_programmers_guide.pdf)
* Scan the Quick Start guides in the [Wiki](https://github.com/frenetic-lang/frenetic/wiki)
* Try some Python examples, see [Example Applications in Python](https://github.com/frenetic-lang/frenetic/wiki/Python-Examples)
* Learn Frenetic programming in OCaml from the [Frenetic Tutorial](http://frenetic-lang.github.io/tutorials/Introduction/)
* Learn the theory behind Frenetic by reading the papers at [http://frenetic-lang.org](http://frenetic-lang.org)
* See examples of production Frenetic-based SDN's at [https://github.com/coscin/coscin-app](https://github.com/coscin/coscin-app) and [https://github.com/coscin/gates](https://github.com/coscin/gates).
* Consult the Frenetic [API documentation](http://frenetic-lang.github.io/frenetic/).

## Contributing

Frenetic is an open source project, and we encourage you to contribute!

* File Issues and Feature Requests in [Github Issues](https://github.com/frenetic-lang/frenetic/issues)
* Join the [Frenetic Mailing List](http://lists.frenetic-lang.org/mailman/listinfo/frenetic-ocaml) for more in-depth guidance

## Credits

See [Frenetic Members](http://frenetic-lang.org/#members) and [Support](http://frenetic-lang.org/#support)

## License

Frenetic is released under the GNU Lesser Public License, version 3.  [Details](https://github.com/frenetic-lang/frenetic/blob/master/LICENSE)
