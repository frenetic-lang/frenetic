#!/bin/bash

# Ensures that all NetCore files in the repository are parsable.

cd `dirname $0`/..
find . -name "*.nc" | xargs ./src/Frenetic.d.byte -parse-only