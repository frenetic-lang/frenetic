Getting Started
===============

In this tutorial, you will learn to program software-defined networks
(SDN) using OpenFlow. The software for this tutorial is available as
a virtual machine. To get started:

- Download and install the [VirtualBox](https://www.virtualbox.org)
  virtualization platform.
  
- Download the
  [Frenetic Tutorial VM](http://www.cs.brown.edu/~arjun/tmp/Frenetic.vdi).

  > Insert the right link.

- Launch the tutorial VM, which will launch a GUI and automatically login
  as `frenetic`. The password for the account is also `frenetic`.

- At a terminal, go to the tutorial directory, check for updates, and
  rebuild the tutorial software:

  ```
  $ cd src/frenetic
  $ git pull
  $ make reinstall
  ```

  > Make `make reinstall` work.

Handy References
----------------

- [Introduction to OCaml](http://www.cs.cornell.edu/courses/cs3110/2012fa/recitations/rec01.html)

  In this tutorial, you will build controllers in OCaml. We use a tiny
  fragment of the language and provide several examples, but a little
  familiarity with OCaml syntax will be helpful.

  We recommend that you either (1) skim the Introduction to OCaml, or
  (2) do this tutorial with a partner who has passing familiarity with
  OCaml (or Haskell, or some related language).

  > Pick a good introduction. I think the 3110 intro has too many words.

- [Ox Platform Reference](http://frenetic-lang.github.io/frenetic/docs/)
  
  You will write your controllers using Ox, which is a lightweight
  library for writing controllers in OCaml. This tutorial will guide you
  though writing Ox controllers.

  Ox is loosely based on controllers such as [POX]
  (https://openflow.stanford.edu/display/ONL/POX+Wiki) and [NOX]
  (http://www.noxrepo.org/nox/about-nox/). You should be able to apply
  what you learn in this tutorial to write POX/NOX controllers too.

- [OpenFlow 1.0 Specification] (http://www.openflow.org/documents/openflow-spec-v1.0.0.pdf)

  The OpenFlow specification describes OpenFlow-conformant switches
  and details the wire-format of the OpenFlow protocol. You'll find that
  most of the Ox Platform Reference simply reflects the OpenFlow messages
  and data types into OCaml.

- [Mininet] (http://mininet.org/walkthrough/)

  You will use the Mininet network simulator to run your
  controllers. We will tell you exactly what Mininet commands to use,
  so you don't need to read this.



[Action]: http://frenetic-lang.github.io/frenetic/docs/OpenFlow0x01.Action.html

[PacketIn]: http://frenetic-lang.github.io/frenetic/docs/OpenFlow0x01.PacketIn.html

[PacketOut]: http://frenetic-lang.github.io/frenetic/docs/OpenFlow0x01.PacketOut.html

[OxPlatform]: http://frenetic-lang.github.io/frenetic/docs/Ox_Controller.OxPlatform.html

[Match]: http://frenetic-lang.github.io/frenetic/docs/OpenFlow0x01.Match.html

[Packet]: http://frenetic-lang.github.io/frenetic/docs/Packet.html
