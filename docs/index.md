---
title: ALPACA docs
...

These pages document the implementation of ALPACA.
Using a small program, we'll see [what ALPACA computes],
see [how it computes this][Alternating Conditional Analysis],
and [step through a run][Stepping through a run] of
ALPACA to name the modules as they are used.
An [index][Index to source files] to the documentation of
individual source files is given at the end.

What ALPACA computes
--------------------

ALPACA is an analysis tool that determines, for some program
and some property-to-be-checked, all the program paths that
*must* or *may* satisfy the property.
(*Must* information guarantees reachability; *may* information
overapproximates and could introduce false positives.)
These program paths are expressed in logical formulae
constraining the values of program inputs.
For example, consider trying to determine the constraints
on `x` and `y` that lead to reaching an `error()` call
in the following C program.

``` c
main() {
  int x=input(); int y=input();

  if (x<0) {
    error();
  } else if ((x>9) && (x<y*y)) {
    error();
  }
}
```

We see that an `error()` can be reached in two ways:
if $x<0$, or if $x>9 \land x<y*y$; and all inputs that
do not satisfy these constraints will avoid any `error()`.
ALPACA runs a host of tools that look for reachability
or unreachability proofs.
If the underlying tools can determine the two reachability
constraints described above as well as provide an unreachability
proof in the remainder of the program (i.e., inputs not
satisfying the two constraints), then ALPACA will return
this exact description.

However, determining that all inputs outside of these
constraints do not lead to an `error()` may be
difficult for an automated static analysis due to the
nonlinear constraint $x<y*y$.
In this case, we may try relaxing the constraint of
$x>9 \land x<y*y$ to be simply $x>9$.
With the nonlinearity removed, the static analyses
can prove that inputs outside of $x<0 \lor x>9$ will
not lead to an `error()`, and the description of
program paths returned by ALPACA becomes slightly
more approximate: an `error()` *must* be reached if
$x<0$, or if $x>9 \land x<y*y$, and *may* be reached
if $x>9$; everything not implied by these formulae
will avoid the `error()`.

This mix of *must* and *may* characterizations is
computed using the alternating conditional analysis
framework, described next.

Alternating Conditional Analysis
--------------------------------

ALPACA is an instantiation of the Alternating Conditional
Analysis (ACA) framework.
ACA builds upon ideas from
conditional model checking [@beyer:2012:conditional]
and alternation between *may* and *must* pieces of
information returned by an
analysis [@godefroid:2010:compositional].
The general idea of ACA is to use many static analysis
tools to iteratively collect either reachability or
unreachability proofs in some part of a program, and to
condition analysis tools to ignore the already-analyzed
parts.

The flow diagram for ACA is given below.

![](pics/flow-diagram.svg)

The above flow diagram is directly reflected in its
implementation within ALPACA.
Below is the recursive `aca` function within
`Analysis.lhs`.

``` haskell
aca :: Program -> Csc -> AcaComputation Csc
aca program csc = do
  result <- exploreSubspace program csc
  case result of
    UnreachableEvidence -> do
      csc' <- enforceDisjointness csc
      lastWrites csc'
      return csc'
    (ReachableEvidence ev) -> do
      csc' <- characterize ev
      program' <- condition program csc'
      aca program' csc'
    NoEvidence -> do
      csc' <- generalize csc
      program' <- condition program csc'
      aca program' csc'
```

Stepping through a run
----------------------

Index to source files
---------------------

The entrypoint of the program is [Main](app/Main.html),
the high-level logic is in [Analysis](src/Analysis.html), and
the rest is here:

* [AcaComputation](src/AcaComputation.html)
* [BinarySearch](src/BinarySearch.html)
* [Branch](src/Branch.html)
* [Characterize](src/Characterize.html)
* [CivlParsing](src/CivlParsing.html)
* [Configuration](src/Configuration.html)
* [CpaParsing](src/CpaParsing.html)
* [CscTypes](src/CscTypes.html)
* [LaunchBenchexec](src/LaunchBenchexec.html)
* [Lib](src/Lib.html)
* [LocalPaths](src/LocalPaths.html)
* [ModularAca](src/ModularAca.html)
* [Octagon](src/Octagon.html)
* [Parsing](src/Parsing.html)
* [Portfolio](src/Portfolio.html)
* [Reading](src/Reading.html)
* [Refine](src/Refine.html)
* [RunPortfolio](src/RunPortfolio.html)
* [Solver](src/Solver.html)
* [SolverLib](src/SolverLib.html)
* [Statistics](src/Statistics.html)
* [Transformations](src/Transformations.html)
* [Transformer](src/Transformer.html)
* [Writing](src/Writing.html)

# References