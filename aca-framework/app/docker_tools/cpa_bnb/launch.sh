#!/bin/bash

# run cpa_bam_bnb
OUTPUT=$(/cpa_bam_bnb/scripts/cpa.sh -svcomp21-bam-bnb -timelimit 900s -disable-java-assertions -setprop cfa.allowBranchSwapping=false -setprop analysis.checkCounterexamples=false -setprop cpa.arg.witness.exportSourcecode=true /alpaca_in/*.c)
# report full result details
echo $OUTPUT
# if witness exists, move it to /alpaca_out
WITNESS="/output/witness.graphml"
if test -f "$WITNESS"; then
    mv "$WITNESS" /alpaca_out
fi
