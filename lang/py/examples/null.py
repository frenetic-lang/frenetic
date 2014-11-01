from netkat import webkat
from netkat.syntax import *

def handler(event):
    print event
    return

def main():
    webkat.update(drop())
    while True:
        handler(webkat.event())

if __name__ == '__main__':
    main ()
