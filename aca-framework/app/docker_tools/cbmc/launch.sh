#!/bin/bash

# run from cbmc dir
cd /cbmc/
# run cbmc
OUTPUT=$(./cbmc /alpaca_in/*.c --32 --graphml-witness /alpaca_out/witness.graphml --propertyfile /PropertyUnreachCall.prp)
# report full result details
echo $OUTPUT
