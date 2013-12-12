#!/bin/bash
cpp $1  | grep -v '^#' | z3 -in -smt2
