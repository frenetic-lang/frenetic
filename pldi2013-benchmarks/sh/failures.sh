#!/bin/bash

grep -c Graceful *.log | grep ":0" | cut -f 1 -d ":"