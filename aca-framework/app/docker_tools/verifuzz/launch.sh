#!/bin/bash

# run verifuzz  
OUTPUT=$(./scripts/verifuzz.py --propertyFile /PropertyUnreachCall.prp /alpaca_in/*.c)
# report full result details
echo $OUTPUT
# if witness exists, move it to /alpaca_out
WITNESS="/verifuzz/witness.graphml"
if test -f "$WITNESS"; then
    cp "$WITNESS" /alpaca_out
fi
