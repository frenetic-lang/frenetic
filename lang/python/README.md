*This repository is not ready for use.*

# python-netkat

Python bindings for the NetKAT policy language and HTTP application API.

## Development

Ensure that all the dependencies for the package are installed. They are listed
in the `requirements.txt` file and they can all be easily installed using pip:

```
pip install -r requirements.txt
```

You can then perform an 'in-place' installation of the library that will
install the library in the currently active
[virtualenv](http://virtualenv.readthedocs.org/en/latest/) or in the global
python installation. (In the latter case, you will have to use `sudo`). Once
installed in this way, any changes that you make to the source files in this
directory will be reflected in the behavior of the installed package. Perform
an 'in-place' installation using the following command:

```
python setup.py develop
```

## Using the API

To create a python-netkat application, you must define two functions: a
function that takes no arguments and will create an initial state object for an
application; and a second event handler that will take a state object as its
first arguments and a JSON representation of a network event that will return
an optional NetKAT policy object. Their types in OCaml would roughly be the
following:

```ocaml
state : unit -> s
handler : s -> e -> policy option
```

Here's an example of a network application that will print all network events
and generate a drop policy.

```python
#!/usr/bin/env python

from netkat.syntax import *
import netkat.flaskapp


def state():
    pass

def handler(_state, event):
    print event
    return drop()

if __name__ == '__main__':
    netkat.flaskapp.create(state, handler).run(host="<host>", port=<port>)
```

To invoke the application from an ocaml controller, use the `create\_http`
applcation constructor with the same parameters you used to create the python
application. You must also provide a default policy.

```ocaml
Async_NetKAT.create_http default_pol "<host>" <port>
```

Then, compose that application with any others that you may need to use and
then run a NetKAT controller using the resultant application.

## Examples

The `examples/` directory contains runnable example applications. To run an
example, use a command similar to the following, which will start listening on
`localhost:5000`.

```
python -m examples.null
```
