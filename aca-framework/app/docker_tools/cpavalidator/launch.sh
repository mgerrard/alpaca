#!/bin/bash

# remove any empty assumptions
sed -i '/<data key="assumption"\/>/d' /alpaca_in/witness.graphml

# run cpa
OUTPUT=$(/cpa/scripts/cpa.sh -timelimit 90s -setprop cfa.allowBranchSwapping=false -setprop cpa.arg.witness.exportSourcecode=true -spec /PropertyUnreachCall.prp -witness /alpaca_in/witness.graphml -witnessValidation -setprop counterexample.export.exportWitness=true /alpaca_in/*.c)
# report full result details
echo $OUTPUT
# if witness exists, move it to /alpaca_out
WITNESS="/output/witness.graphml"
if test -f "$WITNESS"; then
    mv "$WITNESS" /alpaca_out
fi
