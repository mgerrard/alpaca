#!/bin/bash

# change to graves director
cd /graves
# run graves
OUTPUT=$(./scripts/cpa.sh -svcomp22-graves -timelimit 900s -heap 10000M -benchmark -setprop cfa.allowBranchSwapping=false -setprop cpa.arg.witness.exportSourcecode=true /alpaca_in/*.c)
# report full result details
echo $OUTPUT
# if witness exists, move it to /alpaca_out
WITNESS="./output/witness.graphml"
if test -f "$WITNESS"; then
    mv "$WITNESS" /alpaca_out
fi
