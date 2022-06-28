#!/bin/bash

# run from veriabs dir
cd /veriabs/
# run veriabs
OUTPUT=$(./scripts/veriabs --property-file /PropertyUnreachCall.prp /alpaca_in/*.c)
# report full result details
echo $OUTPUT
# if witness exists, move it to /alpaca_out
WITNESS="/veriabs/witness.graphml"
if test -f "$WITNESS"; then
    mv "$WITNESS" /alpaca_out
fi
