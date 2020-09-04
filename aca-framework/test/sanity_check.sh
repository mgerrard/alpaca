#!/bin/bash

# This script just runs through each tool
# (or, if the tool cannot provide safety
# guarantees, then combine it with SeaHorn)
# and checks that it can be used in ALPACA
# to report an exact partition for the
# simple file at alpaca/examples/test.c
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
      echo "FAILURE with portfolio $portfolio"
    fi
}

ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $ROOT
cd ../../examples

run_test "cpaSeq"
run_test "cpaBamBnb"
run_test "cpaBamSlicing"
run_test "interpChecker"
run_test "uAutomizer"
run_test "uKojak"
run_test "uTaipan"
run_test "veriAbs,seahorn"
run_test "cbmc,seahorn"
run_test "twoLs,seahorn"
run_test "depthK"
run_test "esbmcIncr,seahorn"
run_test "esbmcKind,seahorn"
run_test "symbiotic,seahorn"
run_test "smack,seahorn"
run_test "pesco"
