# GuBPI – An Analyzer for Probabilistic Programs to Compute Guaranteed Bounds on the Posterior

GuBPI is a tool for automatically computing guaranteed bounds on the posterior distributions denoted by probabilistic programs, as presented at PLDI 2022 [1].
GuBPI (pronounced "guppy") stands for **Gu**aranteed **B**ounds for **P**osterior **I**nference.

You can read the details in our paper:

> Beutner, Ong, Zaiser. Guaranteed Bounds for Posterior Inference in Universal Probabilistic Programming. PLDI 2022. https://arxiv.org/abs/2204.02948

## What does GuBPI do?

GuBPI enables the computation of sound bounds on the denotation of a program.
The input to GuBPI is a file that contains both the probabilistic program in question (we discuss the syntax of our language, SPCF, below) and additional configuration parameters that determine the depth and precision of the analysis.
GuBPI performs a symbolic analysis of the input program.
In particular, it analyses each symbolic path with respect to an interval-based semantics (see Sections 3 and 4 in [1]).
In case linear optimization is applicable, GuBPI performs analysis with an optimized interval-based semantics (see Section 6.4 in [1]) and uses Vinci to discharge volume computations.

## Structure of the repository

- `src/`: the source code of GuBPI (written in F#).
- `vinci/`: a modified version of the VINCI tool (written in C/C++), used to compute the volume of polytopes.
- `benchmarks/`: examples of probabilistic programs with expected output.
- `plotting/`: a tool to generate prettier plots of the computed bounds. (GuBPI itself produces different plots using XPlot.)


## Build and run GuBPI

### Build GuBPI locally

To make use of the optimized interval-based semantics, GuBPI makes use of [VINCI](https://link.springer.com/chapter/10.1007/978-3-0348-8438-9_6) to compute the volume of a convex polytope.
The call to VINCI is made as a system call, so the vinci executable should be located in the same folder as the GuBPI executable (see the build instructions below).

In order to build this project, you need the .NET 6 SDK (we tested version 6.0.200), a C/C++ Complier (we tested gcc) and GNU make (we tested GNU Make 3.81).
To install make and gcc, you can install the `build-essential` package (on Linux) or Xcode (on macOS).
You can find instructions on how to install the .NET SDK [here](https://docs.microsoft.com/en-us/dotnet/core/install/).

To build GuBPI, run the following.

```shell
cd vinci
make
cd ..
cd src
dotnet build -c "Release" -o ../app
cd ..
cp vinci/vinci ./app
```

Afterward, you can run GuBPI via

```shell
./app/GuBPI inputfile.spcf
```

where `inputfile.spcf` is the (path to the) file to be analysed.
GuBPI will place the output files in the `output/` folder.

**Explanation** The above commands build VINCI (located in `vinci/`) using `make`.
In the second step we build GuBPI located in `src/`and place the executable in the `app/` folder (by running `dotnet build -c "Release" -o ../app`).
Note that the executable (`app/GuBPI`) is **not** standalone and needs the `.dll` files located in the same folder.
In particular, to copy the executable to different location, you need to copy the entire `app/` folder.
In order for GuBPI to run the vinci executable (built in the first step), vinci must be in the same directory as GuBPI (ensured by `cp vinci/vinci ./app`).
Now the tool can be executed by running the `./app/GuBPI` executable provided with the SPCF file to be analyzed.


### GuBPI Docker container

We also provide a Dockerfile to build GuBPI.
This requires a working version of [docker](https://www.docker.com/) (we tested version 20.10.2).
To construct a docker image, run `docker build -t gubpi .`
Afterwards, the image `gubpi` should be listed when you run `docker images`.
If you get a `permission denied` error, you may have to run docker with superuser rights, i.e. `sudo docker ...`.

The Docker image can be used similarly to the executable above.
To run GuBPI on a example program, run `docker run -v $PWD/benchmarks/:/benchmarks/ -v $PWD/output/:/output/ -it gubpi inputfile.spcf` where `inputfile.spcf` is (the path to) the SPCF program for which you want to compute bounds.
Note that for this command the input *must* be (a path to) a file in the `benchmarks/` folder.
The output of GuBPI will, again, be located in the `./output` folder.

### Example
To analyse the problem in `benchmarks/NonRecursive/coinBias/coinBias.spcf`, you can either run

```shell
./app/GuBPI ./benchmarks/NonRecursive/coinBias/coinBias.spcf
```

in case you build GuBPI manually. Or run

```shell
docker run -v $PWD/benchmarks/:/benchmarks/ -v $PWD/output/:/output/ -it gubpi /benchmarks/NonRecursive/coinBias/coinBias.spcf
```

in case you have build the `gubpi` Docker image.

The 5 output files of GuBPI will be located in the `output/` folder (see details on the output below). To have a first look at the bounds computed by GuBPI, open the `coinBias-norm.html` file which displays the plots in your browser.
In order to plot the bounds the way they look in [1], you can use the plotting script in the `plotting/` folder (see `plotting/README.md` for details).



## Input and output of GuBPI

GuBPI analyses programs in a functional language called *Statistical PCF* (SPCF).
The `.spcf` file passed to GuBPI contains both the program to be analysed and the parameters used to guide the analysis by GuBPI.
We explain the syntax of SPCF, the supported hyperparameters and the output below.

### Syntax of SPCF

The syntax of SPCF supports the following features.

- Numerical Constants: `5.3`
- Variables: `x`
- Lambda-Abstraction: `\x. x`
- Fixpoint-Abstraction: `fix f x. x`
- Application: `(\x. x) 4`
- Conditional `if x then y else z`
- Samples from different distributions: `sample uniform(0, 1)`.
  The supported distribution include `uniform(a, b)`, `normal(mean, std_dev)`, `truncnormal(mean, std_dev, left, right)`, `beta(alpha, beta)`.
  Here `truncnormal(mean, std_dev, left, right)` samples from a normal distribution that is truncated to the interval `[left, right]`.
- Scores: `score(4)`
- Primitive Functions: `add(4, 5)`. The supported functions are `neg(x)`, `add(x, y)`, `sub(x, y)`, `mul(x, y)`, `div(x, y)`, `exp(x)`, `log(x)`, `pdfnormal(mean, std_dev, x)`. As syntactic sugar it is also possible to write addition, subtraction and multiplication via `+, -, *` in infix notation.
- Lets: `let x = 3 in x + 5`
- recursive lets: `letrec f x = f x in f 0`
- Tuples: `(|1, 2|)`
- Matches on tuples: `let x, y = (|1, 2|) in x + y`
- Lists: `[1, 1, 0, 1, 0]`, `[1, 2 | restList]`
- Matches on Lists: `match [1, 1, 2] | [] -> 0 | [x | xs] -> x`

Lines beginning with a `#` are comments

The language constructs should be self-explanatory.
Note that SPCF is a strongly typed language and GuBPI enforces that the program is well-typed.
We recommend having a look at the various examples in `benchmarks/` to get an overview.
Note that the performance of GuBPI depends on the structure of the program, the used distributions, and primitive functions.
There are programs that GuBPI cannot currently handle.


### Supported configuration parameters

Comment lines at the beginning of a `.spcf` file determine the the configuration parameters for GuBPI.
The following options are available:

- `# method boxes`: forces GuBPI to use the pure interval-based semantics.
  If this option is not set, GuBPI uses the linear optimization when applicable and otherwise resorts to the pure interval-based semantics.
  If `# method boxes` is set, `# splits 100000` gives the number of splits (here: 100000) that should be performed by GuBPI.
- `# depth 20`: specifies the depth of the symbolic exploration (here: 20).
- `# discretization -5 5 0.25`: specifies the histogram bins for which we would like to obtain bounds.
  The first number is the left bound, the second number the right bound and the third number is the step size (the size of each bucket).
  So the example `# discretization -5 5 0.25` will analyse the buckets `[-5, -4.75], [-4.75, -4.5], ..., [4.75, 5]`.
- `# epsilonScore 0.1`: sets the precision with which to analyse the scoring in the optimized semantics.
  Each score statement is partition into boxes of size `epsilonScore`. The smaller `epsilonScore`, the finer the scores are split.
- `# epsilonVar 0.2`: sets the precision with which to analyse sample variables from non-uniform distributions in the optimized semantics.
  The smaller `epsilonVar`, the finer the variables are split.

- `# outputSplitProgress 0`, `# outputCurrentPath 0`, `# outputCurrentArea 0`: control the amount of progress information GuBPI prints to the terminal, i.e., outputs which symbolic path is currently analysed (`outputCurrentPath`), which bucket of the discretization is analysed (`outputCurrentArea`) and what split of the variables is currently worked on (`outputSplitProgress`). A `0` turns the output off, a `1` turns it on.

If no parameters are set, GuBPI uses the default ones.

The parameters have a direct impact on the speed and results of GuBPI.
If you increase the `depth` and decrease `epsilonScore` and `epsilonVar`, then precision will increase (but so will the running time).
Making them too small may cause GuBPI to fail to terminate within a reasonable amount of time.
For the benchmarks from the paper, we already provide good values for those parameters in the `.spcf` files.

### Output

When called on an SPCF program `{problem}.spcf`, GuBPI will output 5 files.

- `{problem}-unnorm.bounds`: contains the bounds for the unnormalized denotation computed for each bucket.
- `{problem}-norm.bounds`: contains the bounds for the normalized denotation computed for each bucket.
- `{problem}-unnorm.html`: contains a plot of the bounds on the unnormalized denotation (using XPlot, a plotting library in F#).
  The plots from the paper can be generated from the bounds file using the tool in the `plotting/` folder.
- `{problem}-norm.html`: similar to `{problem}-unnorm.html` but using the normalized denotation.
- `{problem}.clj`: contains a translation of the SPCF term to an Anglican program.
  [Anglican](https://probprog.github.io/anglican/index.html) is a probabilistic programming language implemented in Clojure.
  This allows one to experiment with Anglican's inference algorithms on the example programs without the need for a manual translation.

## References

[1] Beutner, Ong, Zaiser. Guaranteed Bounds for Posterior Inference in Universal Probabilistic Programming. PLDI 2022. https://arxiv.org/abs/2204.02948
