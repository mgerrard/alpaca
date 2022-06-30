#!/bin/bash

# run from esbmc dir
cd /esbmc/
# run esbmc
OUTPUT=$(./esbmc-wrapper.py -s kinduction -p PropertyUnreachCall.prp -p /PropertyUnreachCall.prp -a 32 /alpaca_in/*.c)
# report full result details
echo $OUTPUT

mv /esbmc/*.graphml /alpaca_out/witness.graphml
