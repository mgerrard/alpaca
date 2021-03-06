#!/bin/bash

# MAKE SURE MOUNTED DIRECTORY IS WRITEABLE
MOUNTED_DIR=/home/alpaca_logs
# 1. find the $XML_FILE at $MOUNTED_DIR
XML_FILE=( $MOUNTED_DIR/*.xml )
# 2. infer the $TOOL from the $XML_FILE
TOOL=$(basename "$XML_FILE" | cut -d. -f1)
if [ $TOOL == "CPA_Validator" ]; then
  TOOL="CPA_Seq"
fi
# 3. go to that $TOOL's directory
cd $TOOL
# 4. run benchexec from the $XML_FILE
benchexec --full-access-dir / $XML_FILE
# 5. if witness exists, move it to the results directory
WITNESS=`find . -name "*witness.graphml"`
if [ -n "$WITNESS" ]; then cp --no-preserve=mode $WITNESS results; fi
# 6. move the results directory to the mounted directory
cp --no-preserve=mode -r results $MOUNTED_DIR
