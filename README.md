# Welcome to Frenetic

[![Build Status](https://travis-ci.org/frenetic-lang/frenetic.svg?branch=master)](https://travis-ci.org/frenetic-lang/frenetic)

Frenetic is an open-source Software Defined Network (SDN) controller platform designed to make SDN programming easy, modular, and semantically correct.

The languages used to program networks today lack modern features. Programming them is a complicated and error-prone task, and outages and infiltrations are frequent. Frenetic is a _network programming language_ with the following essential features:

* _High-level abstractions_ that give programmers direct control over the network, allowing them to specify what they want the network to do without worrying about how to implement it.
* _Modular constructs_ that facilitate compositional reasoning about programs.
* _Portability_, allowing programs written for one platform to be reused with different devices.
* _Rigorous semantic foundations_ that precisely document the meaning of the language and provide a solid platform for mechanical program analysis tools.

You can build Frenetic-based network applications with:

* Python
* OCaml
* REST and JSON (= any programming language!)

## Getting Started

### Installation

1. Install VirtualBox from https://www.virtualbox.org/wiki/Downloads. Use the latest
version platform package appropriate for your system.
2. From https://s3.amazonaws.com/plasma-umass/frenetic-tutorial-vm.ova download
the latest Frentic User VM – this file is about 2.8 GB and may take about 10
minutes or so to download.
3. Import the .ova file into VirtualBox. This takes two minutes or so.

### Hello SDN World

1.  Start up a terminal window - – two are provided in the VM under Accessories: Byobu Terminal (which integrates nicely with tmux) and LXTerminal (which has graphical tabs). Either one will do.
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

## Contributing

Frenetic is an open source project, and we encourage you to contribute!

* File Issues and Feature Requests in [Github Issues](https://github.com/frenetic-lang/frenetic/issues)
* Join the [Frenetic Mailing List](http://lists.frenetic-lang.org/mailman/listinfo/frenetic-ocaml) for more in-depth guidance

## Credits

See [Frenetic Members](http://frenetic-lang.org/#members) and [Support](http://frenetic-lang.org/#support)

## License

Frenetic is released under the GNU Lesser Public License, version 3.  [Details](https://github.com/frenetic-lang/frenetic/blob/master/LICENSE)