#!/bin/bash

grep -Ev '^(n|l)' $1 | grep '#' | sed -e 's/^[ \t]*//'
