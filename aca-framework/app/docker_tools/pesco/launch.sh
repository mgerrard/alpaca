#!/bin/bash

# run pesco
OUTPUT=$(/pesco/scripts/cpa.sh -svcomp21-pesco -benchmark -stack 2048k -heap 10000M -timelimit 900s -setprop cfa.allowBranchSwapping=false -setprop cpa.arg.witness.exportSourcecode=true /alpaca_in/*.c)
# report full result details
echo $OUTPUT
# if witness exists, move it to /alpaca_out
WITNESS="/output/witness.graphml"
if test -f "$WITNESS"; then
    mv "$WITNESS" /alpaca_out
fi
