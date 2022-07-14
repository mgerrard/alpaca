#!/bin/bash

cd /infer
# run infer
OUTPUT=$(/infer/infer-wrapper.py --property /PropertyUnreachCall.prp --program /alpaca_in/*.c)
# report full result details
echo $OUTPUT
