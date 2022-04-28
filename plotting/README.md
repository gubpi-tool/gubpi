# Plotting

This directory contains a Julia script to create better-looking plots of the bounds computed by GuBPI.
The plots look as in [1].

## Setup/Installation

You need to have [Julia](https://julialang.org/downloads/) and [Jupyter](https://jupyter.org/install) installed on your system, as well as the Julia packages `IJulia`, `Plots` and `StatsPlots`.
These packages can be installed as follows:

```shell
$ julia
> ]
> add IJulia
> add Plots
> add StatsPlots
```

## Plotting a `.bounds` file

To plot a `.bounds` file, run:

```shell
julia plotbounds.jl inputfile.bounds outputfile.pdf
```

where `inputfile.bounds` is the bounds file that should be plotted.


### Example
For example, if you computed the file `output/coinBias-norm.bounds` and your current directory is `plotting/` simply run

```shell
julia plotbounds.jl ../output/coinBias.bounds coinBias-plotted.pdf
```

to produce the `coinBias-plotted.pdf` plot in the `plotting/` folder.

## References

[1] Beutner, Ong, Zaiser. Guaranteed Bounds for Posterior Inference in Universal Probabilistic Programming. PLDI 2022. https://arxiv.org/abs/2204.02948
