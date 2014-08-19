import netkat.flaskapp
from netkat.syntax import *

def state(_):
    pass

def handler(_, event):
    print event
    return drop()

if __name__ == '__main__':
    netkat.flaskapp.create(state, handler).run()
