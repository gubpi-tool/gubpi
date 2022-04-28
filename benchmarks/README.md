# Benchmark Overview

This folder contains a selection of benchmarks for GuBPI (including those used for the evaluation in [1]).

The following sets of benchmarks exist (each located in a separate folder):

- `ProbEstimation`: benchmarks that estimate the probability of an event (adapted from [2]).
There is a separate folder for each program.
The `.i` file is the original program (from [2]) and the `.prop` file contains the corresponding list of probability estimation problems on that program.
The `.spcf` file is the translation of the program and property into SPCF.
In the SPCF program, the query is already included in the program as follows: if we wish to estimate the probability of `\phi` at the end of the program, we insert the condition `if \phi then 0 else 1` at the end of the SPCF program.
Then compute upper and lower bounds on the output 0 (in the programs we check the denotation on the interval `[-0.1, 0.1]`).
Each query is therefore given as a separate program. The bounds computed for the interval `[-0.1,0.1]` correspond to the probability of the event.

- `Discrete`: discrete benchmarks taken from PSI [3,4].
Each example is located in a separate folder.
Each folder contains the original `.psi` file taken from the PSI repository [4], as well as the translation into SPCF (the `.spcf` file).
The comment at the top of the `.spcf` file gives the exact location of the PSI file in the PSI repository.
As the output of each program is binary (either 0 or 1), each query asks GuBPI for bounds on the interval [-0.1, 0.1].
The bounds of falling within those bounds is the posterior weight of output 0 and the weight outside [-0.1, 0.1] that of output 1.

- `NonRecursive`: non-recursive models (including examples from the literature). Each program is located in a separate folder as a `.spcf` file.

- `Recursive`: recursive models (including examples from the literature). Each program is located in a separate folder as a `.spcf` file.

- `FurtherExamples`: a selection of further examples (both recursive and non-recursive).


## References

[1] Beutner, Ong, Zaiser. Guaranteed Bounds for Posterior Inference in Universal Probabilistic Programming. PLDI 2022. https://arxiv.org/abs/2204.02948

[2] Sankaranarayanan, Chakarov, Gulwani. Static analysis for probabilistic programs: inferring whole program properties from finitely many paths. PLDI 2013.

[3] Gehr, Misailovic, Vechev. PSI: Exact Symbolic Inference for Probabilistic Programs. CAV 2016.

[4] PSI Solver: https://psisolver.org/
