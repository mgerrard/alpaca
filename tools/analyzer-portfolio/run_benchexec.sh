#!/bin/bash

CURRENT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

C_FILE=$1
XML_FILE=$2
OUTPUT_DIR=$3
ANALYSIS_DIR=$4
TIMEOUT=$5

cd $ANALYSIS_DIR

# This strange check is to allow BenchExec to play nicely with the setup
# on UVa's doppio servers, whose /net directory mess things up
if [ -d "/net" ];
then
  benchexec $XML_FILE --hidden-dir /net -o $OUTPUT_DIR -T $TIMEOUT
else
  benchexec $XML_FILE -o $OUTPUT_DIR -T $TIMEOUT
fi

cd $CURRENT_DIR
