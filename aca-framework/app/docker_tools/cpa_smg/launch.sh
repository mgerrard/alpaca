#!/bin/bash

# run cpa-bam-smg
OUTPUT=$(/cpa_bam_smg/scripts/cpa.sh -svcomp22-bam-smg -timelimit 900s -disable-java-assertions -setprop cfa.allowBranchSwapping=false -setprop analysis.checkCounterexamples=false -setprop cpa.arg.witness.exportSourcecode=true /alpaca_in/*.c)
# report full result details
echo $OUTPUT
# if witness exists, move it to /alpaca_out
WITNESS="/output/witness.graphml"
if test -f "$WITNESS"; then
    mv "$WITNESS" /alpaca_out
fi
