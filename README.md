# SymEx Triangle

A customized symbolic execution engine for reasoning about SSHOMs and
strict-SSHOMs in the Triangle program. Ideas and main results are discussed in
the Appendix of the following paper:

>Chu-Pan Wong, Jens Meinicke, Leo Chen, João P. Diniz, Christian Kästner,
>and Eduardo Figueiredo. 2020. Efficiently Finding Higher-Order Mutants. In
>Proceedings of the 28th ACM Joint European Software Engineering Conference
>and Symposium on the Foundations of Software Engineering (ESEC/FSE ’20),
>November 8–13, 2020, Virtual Event, USA.ACM, New York, NY, USA, 13 pages.
>https://doi.org/10.1145/3368089.3409713

The symbolic execution part is based on [Scala Parser
Combinator](https://github.com/scala/scala-parser-combinators). Current
implementation has very limited support for Java language features, but should
work fine for our study subject **Triangle** or other similar programs that
mainly use basic arithmetic computations and simple branching. We use
[Z3](https://github.com/Z3Prover/z3) to solve constraints. 

# Usage

Install [Z3](https://github.com/Z3Prover/z3) and
[sbt](https://www.scala-sbt.org/download.html), and run as a sbt project. Note
that we took the Triangle program verbatim from prior work. There is a known bug in the
Triangle program, but we use it as it is to have comparable results with
previous work. The project is structured as follow.

```plain
.
├── README.md
├── build.sbt
├── results
│   ├── sshom-bf2.txt                // all second-order SSHOMs, w.r.t. all possible tests
│   ├── sshom-bf3.txt                // all third-order SSHOMs, w.r.t. all possible tests
│   ├── sshom-varex.txt              // SSHOMs found by search_var and verified by symbolic execution to generalize to all possible tests (see Section 3)
│   ├── strict-sshom-bf2.txt         // all second-order strict-SSHOMs, w.r.t. all possible tests
│   └── strict-sshom-varex.txt       // strict-SSHOMs found by search_var and verified by symbolic execution to generalize to all possible tests (see Section 3)
├── src
│   └── main
│       └── scala
│           └── JavaParser.scala     
├── sshom-varex-unverified.txt
├── triangle-mutant-group.txt        // FOMs that cannot be combined 
├── triangle.txt                     // the mutated triangle program with 128 FOMs
└── fun.txt                          // functions called by the triangle program
```
