#!/bin/bash

# must be run from uk dir
cd /uk
# run ua
OUTPUT=$(./Ultimate.py --full-output --spec /PropertyUnreachCall.prp --architecture 32bit --witness-dir /alpaca_out --file /alpaca_in/*.c)
# report full result details
echo $OUTPUT
