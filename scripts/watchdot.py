#!/usr/bin/env python

'''This script watches a given directory (by default the current one) for
changes to DOT files and uses the dot utility (part of graphviz) to generate a
visualization in PDF format
'''

import os
import sys
import time
import subprocess

from watchdog.observers import Observer
from watchdog.events import PatternMatchingEventHandler

CMD = ['dot', '-Tpdf']

class Handler(PatternMatchingEventHandler):
    patterns = ["*.dot"]

    def to_pdf(self,path):
        """ Turn the dot file into the corresponding PDF file
        """
        name = os.path.splitext(path)[0]
        cmd = CMD + [path] + ["-o"+name+".pdf"]

        print cmd
        try:
            p = subprocess.Popen(cmd,stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        except OSError as e:
            print e
            exit(1)

        out,err = p.communicate()
        return

    def on_modified(self, event):
        self.to_pdf(event.src_path)

    def on_created(self, event):
        self.to_pdf(event.src_path)


if __name__ == '__main__':
    observer = Observer()
    path = sys.argv[1] if len(sys.argv) > 1 else '.'
    print "Running on path", path
    observer.schedule(Handler(), path=path)
    observer.start()

    try:
        while True:
            time.sleep(1)

    except KeyboardInterrupt():
        observer.stop()

    observer.join()
