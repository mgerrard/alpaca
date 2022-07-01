#!/bin/bash

# run from symbiotic dir
cd /symbiotic/
# run symbiotic
OUTPUT=$(./bin/symbiotic --witness /alpaca_out/witness.graphml --32 --sv-comp /alpaca_in/*.c)
# report full result details
echo $OUTPUT
