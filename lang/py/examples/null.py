import netkat.flaskapp
from netkat.syntax import *

def state():
    pass

def handler(_, event):
    print event
    return drop()

if __name__ == '__main__':
    app = netkat.flaskapp.create(state, handler)
    app.debug = True
    app.run()
