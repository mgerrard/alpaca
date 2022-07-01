#!/bin/bash

# run from smack dir
cd /smack/
# run smack
OUTPUT=$(./smack.sh -w /alpaca_out/witness.graphml /alpaca_in/*.c)
# report full result details
echo $OUTPUT
