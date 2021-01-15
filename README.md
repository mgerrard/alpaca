This is an instantiation of the 
*Alternating Conditional Analysis* framework.

---

### BenchExec and Ubuntu interaction bug

An Ubuntu update in October 2020 broke container functionality in BenchExec, documented [here](https://github.com/sosy-lab/benchexec/issues/621).
To work around this, I am running in `--docker` mode. Unfortunately this requires a `sudo` command every time due to launching `docker` in
priviledged mode (necessary for the container-within-container, i.e., BenchExec within Docker, solution). Typing `sudo` every time is annoying,
so I tell my system to stop asking just for the ALPACA command by running `sudo visudo` and adding this line to the end:
`mjg6v ALL=(ALL) NOPASSWD:SETENV: /home/mjg6v/.local/bin/alpaca`. I set up an alias for running ALPACA under the name of `aca` by adding
this line to my `~/.bashrc` file: `alias aca="sudo -E /home/mjg6v/.local/bin/alpaca --docker --dse cpa"`.

### Links

- [2019 tool paper](https://bitbucket.org/mgerrard/alpaca/raw/81184644b101f170ce74713fc99b8a6a348ef94f/aca-framework/doc/icse_2019_tool.pdf)
  describing ALPACA
- [2019 video demonstration](https://youtu.be/H2yXtvODurQ) of ALPACA
- [2017 technical paper](https://bitbucket.org/mgerrard/alpaca/raw/81184644b101f170ce74713fc99b8a6a348ef94f/aca-framework/doc/ase_2017_technical.pdf) describing the framework
  behind ALPACA 
      + Note that the current framework of an
        *Alternating Conditional Analysis* is a generalization
        of the one described in this paper, which is named
        *Alternating Characterization of Failures*. Similarly,
        the structure of a *Comprehensive State Characterization*
        used in ALPACA is a generalization of a 
        *Comprehensive Failure Characterization*,
        described in the 2017 paper.
        
### Underlying Static Analysis Tools

* [CPAchecker](https://cpachecker.sosy-lab.org/)
* [UltimateAutomizer](https://ultimate.informatik.uni-freiburg.de/)
* [Symbiotic](https://github.com/staticafi/symbiotic)
* [ESBMC](http://esbmc.org/)
* [PeSCo](https://link.springer.com/chapter/10.1007/978-3-030-17502-3_19)
* [VeriAbs](https://link.springer.com/chapter/10.1007/978-3-319-89963-3_32)
* [2LS](https://github.com/diffblue/2ls)
* [CBMC](https://github.com/diffblue/cbmc)
* [UltimateKojak](https://ultimate.informatik.uni-freiburg.de/)
* [UltimateTaipan](https://ultimate.informatik.uni-freiburg.de/)
* [Pinaka](https://github.com/sbjoshi/Pinaka)

---

#### Directory structure

The source for this project is in
`./aca-framework/src`, and the top-level
driver is in `./aca-framework/app/Main.hs`. 
We use a C parsing
library (extended so that AST nodes can
be compared for equality) that is in
`./language-c`.  Both `aca-framework`
and `language-c` follow the structuring
conventions of a Haskell project
managed by the build tool `stack`; a
lot of the files in these directories
are just boilerplate. The analysis
tools included in the portfolio
are in `./tools/analyzer-portfolio`. 
Small examples of
C programs with embedded assertions
are in `./examples`.
The testing infrastructure is in
`./aca-framework/test/`.

#### Installation

To install `alpaca` on your machine,
run `./install.sh` from a shell; it
will tell you if you're missing any
dependencies.
(See the dependencies section below.)
Be sure to add "~/.local/bin" 
to your PATH variable so that you can 
call `alpaca` from any directory. 
To do so, you can edit your ~/.bashrc file 
to include the line 
`export PATH=$PATH:$HOME/.local/bin`.

#### Building

If you edit any source in
`./aca-framework`, you can rebuild
the `alpaca` executable by moving to
`./aca-framework` (or one of its child
directories) and running
`stack install --fast`.

#### Testing

The full documentation for testing is in
`./aca-framework/test/`.
You can run tests and generate a coverage report by 
moving to `./aca-framework` (or one of its child
directories) and running
`stack test --fast --test --coverage aca-framework`.
At the end of the test run, Stack will report where it placed the report, 
in some very long path name, e.g.,
`/home/mgerrard/work/alpaca/aca-framework/.stack-work/install/x86_64-linux/lts-9.17/8.0.2/hpc/aca-framework/aca-framework-test/hpc_index.html`.
(*If the tests can run, but no coverage report is 
generated, run* `stack clean`, *followed by the preceding
command.*)
The coverage report will be an HTML page viewable
by any browser, such as Firefox.

#### Dependencies
##### Ubuntu Packages
+ `libc6-dev-i386`
+ `python-pycparser`
+ `haskell-stack` (Be sure to run `stack upgrade --binary-only` afterwards.)
+ `gcc-multilib `
+ `g++-multilib`
+ `libgmp-dev`
+ `libtinfo-dev`

(You can install the above packages by running `sudo apt install <package>`.)

##### Programs
+ `bash`
+ `java`
+ `python2` (some tools' wrapper scripts use constructs only legal in python2)
+ `python3`
+ `benchexec`

##### SMT Solvers

ALPACA uses the SBV library to perform queries on logical formulae. SBV is an interface
designed to work well with multiple SMT solvers.
The default solver in SBV is [Z3](https://github.com/Z3Prover/z3).
You must have at least one of the following
solvers installed with its corresponding executable on your $PATH:

+ `abc`
+ `boolector`
+ `cvc4`
+ `mathsat`
+ `yices`
+ `z3`

Details about specific versions SBV may expect can be found here: 
https://github.com/LeventErkok/sbv/blob/master/SMTSolverVersions.md.

##### Installing BenchExec

The following instructions are taken from the BenchExec installation instructions for Ubuntu; for other systems, please refer to the [installation documentation](https://github.com/sosy-lab/benchexec/blob/master/doc/INSTALL.md).

1. Download BenchExec's `.deb` [package file](https://github.com/sosy-lab/benchexec/releases/download/2.7/benchexec_2.7-1_all.deb).
2. Change directories to the folder containing the downloaded `.deb` file and run:
    + `sudo apt install --install-recommends ./benchexec_*.deb`
3. BenchExec also automatically configures the necessary cgroup permissions. Just add your user to the group `benchexec`:
    + `sudo adduser <USER> benchexec`
4. Reboot to effect the cgroup changes.
5. After installing BenchExec and setting up the cgroups file system, please run:
    + `python3 -m benchexec.check_cgroups`
    + If running the preceding command does not print out warnings, BenchExec should be successfully installed on your machine. 
      This command will report warnings and exit with code 1 if something is missing. 
      Don't worry about complaints about swap memory, e.g. setting `swapaccount=1`; 
      we don't care about precise memory measurements in our case. 
      If you find that something does not work, please check the list of solutions found at the bottom of the [installation page](https://github.com/sosy-lab/benchexec/blob/master/doc/INSTALL.md). 

---

### Acknowledgements

This material is based in part upon work supported by the National Science Foundation under Awards 1617916 and 1901769 and by the Army Research Office under Award W911NF1910054.
