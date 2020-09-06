#!/bin/bash

function check_for {
    command -v $1 >/dev/null 2>&1 || { echo >&2 "I require $1 but it's not installed.  Aborting."; exit 1; }
}

function check_for_python_3_6 {
    command -v "python3.6" >/dev/null 2>&1 || { echo >&2 "Please create a symbolic link from python3.6 to your version of python3 (e.g., 3.8) with the command 'sudo ln -s /usr/bin/python3.8 /usr/bin/python3.6'.  Some tools in the portfolio look explicitly for python3.6, unfortunately. Aborting."; exit 1; }
}

function check_for_python {
    command -v "python" >/dev/null 2>&1 || { echo >&2 "Please create a symbolic link from python to your version of python2 (e.g., 2.7) with the command 'sudo ln -s /usr/bin/python2.7 /usr/bin/python'.  Some tools in the portfolio have a wrapper script that uses constructs only legal in python 2, unfortunately. Aborting."; exit 1; }
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

function check_benchexec_version {
    benchexec_version=$(benchexec --version | awk '{print $2}')
    verlt $benchexec_version 2.7 && {
	echo "Please update your version of benchexec to at least 2.7 by downloading the latest .deb file listed at github.com/mgerrard/alpaca/README.md and following the install instructions.";
	exit 1;
    }
}

function does_package_exist {
    dpkg -l $1 >/dev/null 2>&1 || { echo >&2 "I cannot find $1 in your package manager; please install it. Aborting."; exit 1; }
}

check_for_stack
check_stack_version
check_for "java"
check_for "python3"
check_for_python_3_6
check_for_python
check_for "benchexec"
check_benchexec_version
check_for "z3"
does_package_exist "libc6-dev-i386"
does_package_exist "python-pycparser"
does_package_exist "libgmp-dev"

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

function unpack_and_rename_zip_if_needed {
    # first: canonical folder name (e.g., CPA_Seq)
    # second: zip file name (e.g., cpa-seq)
    # third: unpacked folder name (e.g., CPAchecker-1.6.23-svn\ 26773-unix)
    if [ ! -d "$1" ]; then
	echo "-> unpacking $1"
	if [[ $1 == "InterpChecker" ]] || [[ $1 == "VeriAbs" ]]; then
	    unzip -q $2.zip
        else
	    unzip -q $2.zip && mv "$3" $1;
	fi
    fi
}

ARCHIVE_REPO="https://gitlab.com/sosy-lab/sv-comp/archives-2020/raw/svcomp20/2020"

function download_zip_if_needed {

    zip_file="$1.zip"
    expected_size="$2"
    unpacked_name="$3"
    download_link="$ARCHIVE_REPO/$zip_file"
     
    if [ ! -f "$zip_file" ]; then
	echo "i didnt find $zip_file in $SV_ARCHIVES"
	echo "downloading it from $download_link"
	wget $download_link
    else
	actual_size=$(stat -c %s "$zip_file")
        if [ $actual_size -eq $expected_size ]; then
	    echo "you have the latest $zip_file, moving on..."
	else
	    echo "removing stale $zip_file"
#	    rm -f $zip_file
	    echo "removing possible stale unpacked folder"
#	    rm -rf $unpacked_name

	    echo "downloading new $zip_file from $download_link"
       	    wget $download_link	    
	fi
    fi
}

echo
echo "Downloading SVCOMP archives"
echo
cd $SV_ARCHIVES

download_zip_if_needed "cpa-seq" "102733640" "CPA_Seq"
download_zip_if_needed "uautomizer" "84313069" "UAutomizer"
download_zip_if_needed "symbiotic" "73653236" "Symbiotic"
download_zip_if_needed "esbmc" "39176877" "ESBMC"
download_zip_if_needed "pesco" "289247537" "Pesco"
download_zip_if_needed "veriabs" "313055755" "VeriAbs"
download_zip_if_needed "2ls" "3983396" "TwoLS"
download_zip_if_needed "cbmc" "6832746" "CBMC"
download_zip_if_needed "utaipan" "84306647" "UTaipan"
download_zip_if_needed "ukojak" "84301613" "UKojak"
download_zip_if_needed "cpa-bam-bnb" "101242097" "CPA_BAM_BnB"
download_zip_if_needed "pinaka" "50696530" "Pinaka"

echo
echo "Unpacking any zipped SVCOMP archives"

unpack_and_rename_zip_if_needed "CPA_Seq" "cpa-seq" "CPAchecker-1.9-unix"
unpack_and_rename_zip_if_needed "UAutomizer" "uautomizer" "UAutomizer-linux"
unpack_and_rename_zip_if_needed "Symbiotic" "symbiotic" "symbiotic"
unpack_and_rename_zip_if_needed "ESBMC" "esbmc" "esbmc"
unpack_and_rename_zip_if_needed "Pesco" "pesco" "PeSCo-1.8.2"
unpack_and_rename_zip_if_needed "VeriAbs" "veriabs"
unpack_and_rename_zip_if_needed "TwoLS" "2ls" "2ls"
unpack_and_rename_zip_if_needed "CBMC" "cbmc" "cbmc"
unpack_and_rename_zip_if_needed "UTaipan" "utaipan" "UTaipan-linux"
unpack_and_rename_zip_if_needed "UKojak" "ukojak" "UKojak-linux"
unpack_and_rename_zip_if_needed "CPA_BAM_BnB" "cpa-bam-bnb" "cpa-bam-bnb"
unpack_and_rename_zip_if_needed "Pinaka" "pinaka" "pinaka"

cd $ROOT
echo
echo "Now running a sanity check on each tool; this may take some time..."
./aca-framework/test/sanity_check.sh
echo
echo "Please report a bug if there are any above failures."
echo
echo "To see options (assuming $HOME/.local/bin is on your PATH), run:"
echo
echo "  alpaca -h"
echo
echo "To test out ALPACA (with debugging on) on a single analyzer, run:"
echo
echo "  cd examples; alpaca -d full -p cpaSeq test.c"
echo
