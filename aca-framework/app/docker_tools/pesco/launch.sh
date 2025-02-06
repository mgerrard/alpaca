#!/bin/bash

# run pesco
OUTPUT=$(/pesco/bin/pesco --spec /PropertyUnreachCall.prp --memory 10000M --timelimit 900 /alpaca_in/*.c)
# report full result details
echo $OUTPUT
# if witness exists, move it to /alpaca_out
WITNESS="/output/witness.graphml"
if test -f "$WITNESS"; then
    mv "$WITNESS" /alpaca_out
fi
