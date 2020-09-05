#!/bin/bash

# This script just runs through each tool
# and checks that it can be used in ALPACA
# to report an exact partition for the
# simple file at alpaca/examples/test.c
# (or moves to top, in the case of Symbiotic)
#
# This is just to kick the tires.

function run_test {
    portfolio=$1
    alpaca -p $portfolio test.c > /dev/null 2>&1
    result=`head -2 logs_alpaca/test/exit.summary | tail -1`
    if [ $result == "exact" ]
    then
      echo "success with $portfolio"
    else
	if [ $portfolio == "symbiotic" ] && [ $result == "top" ]; then
            echo "success with $portfolio"
	else
	    echo "FAILURE with portfolio $portfolio"
	fi
    fi
}

ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $ROOT
cd ../../examples

run_test "cpaSeq"
run_test "uAutomizer"
run_test "veriAbs"
run_test "esbmc"
run_test "symbiotic"
run_test "pesco"
