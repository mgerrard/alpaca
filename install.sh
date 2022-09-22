#!/bin/bash

function check_for {
    command -v $1 >/dev/null 2>&1 || { echo >&2 "I require $1 but it's not installed.  Aborting."; exit 1; }
}

verlte() {
    [  "$1" = "`echo -e "$1\n$2" | sort -V | head -n1`" ]
}

verlt() {
    [ "$1" = "$2" ] && return 1 || verlte $1 $2
}

function check_for_stack {
    command -v "stack" >/dev/null 2>&1 || { echo >&2 "I require 'stack' but it's not installed. Please run 'sudo apt install haskell-stack && stack upgrade --binary-only'"; exit 1; }
}

function check_stack_version {
    stack_version=$(stack --version | awk '{print $2}' | sed 's/.$//')
    verlt $stack_version 2.3.3 && {
	echo "Please update your version of stack to at least 2.3.3 by running 'stack upgrade --binary-only'";
	exit 1;
    }
}

function check_java_version {
    java_version=$(java -version 2>&1 | head -n 1 | awk -F '"' '{print $2}')
    verlt $java_version 11 && {
	echo "Please install Java 11; this version is required by CPA's symbolic executor.";
	exit 1;
    }
}

check_for_stack
check_stack_version
check_java_version
check_for "z3"
check_for "git"
check_for "docker"

ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ANALYZERS="$ROOT/tools/analyzer-portfolio/"
SV_ARCHIVES="$ANALYZERS/svcomp_archives/"
ACA_CONFIG="$HOME/.aca.config"
ACA_ROOT="$ROOT/aca-framework"

cd $ACA_ROOT
stack build # Build the Haskell executables
stack install --fast # Install alpaca on your system
cd $ROOT

if [ -f $ACA_CONFIG ]
then
    echo
    echo " ! A file named $CONFIG_FILE already exists"
    echo " ! Renaming the existing '$ACA_CONFIG' to '$ACA_CONFIG.old'"
    echo
    mv $ACA_CONFIG $ACA_CONFIG.old
fi
echo "Writing ALPACA configuration to $ACA_CONFIG"
echo "$ANALYZERS" > $ACA_CONFIG
echo
echo "Configuring CIVL with the available solvers"
CIVL_DIR="$ANALYZERS/civl"
CIVL_JAR="$CIVL_DIR/civl.jar"
java -jar $CIVL_JAR config

cd $ROOT
echo
echo "Set up CPA's Docker image by moving to ./aca-framework/app/docker_tools/cpa and running:"
echo "  sudo docker build cpa ."
echo
echo "Set up ESBMC's Docker image by moving to ./aca-framework/app/docker_tools/esbmc and running:"
echo "  sudo docker build esbmc ."
echo
echo "Now try running `alpaca -p cpaSeq,esbmc ./examples/demo.c`"
echo
