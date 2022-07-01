#!/bin/bash

# run from pinaka dir
cd /pinaka/
# run pinaka
OUTPUT=$(./pinaka-wrapper.sh --graphml-witness /alpaca_out/witness.graphml --propertyfile /PropertyUnreachCall.prp --32 /alpaca_in/*.c)
# report full result details
echo $OUTPUT
