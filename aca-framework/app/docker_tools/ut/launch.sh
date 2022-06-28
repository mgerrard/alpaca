#!/bin/bash

# must be run from ut dir
cd /ut
# run ua
OUTPUT=$(./Ultimate.py --full-output --spec /PropertyUnreachCall.prp --architecture 32bit --witness-dir /alpaca_out --file /alpaca_in/*.c)
# report full result details
echo $OUTPUT
