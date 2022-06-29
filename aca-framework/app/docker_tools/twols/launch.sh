#!/bin/bash

# run from 2ls dir
cd /2ls/
# run 2ls
OUTPUT=$(./2ls /alpaca_in/*.c --32 --graphml-witness /alpaca_out/witness.graphml --propertyfile /PropertyUnreachCall.prp)
# report full result details
echo $OUTPUT
