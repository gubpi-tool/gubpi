if length(ARGS) < 2
    println("Usage: julia plotbounds.jl inputfile.bounds outputfile.pdf")
    return
else
    println("Please wait...")
    include("plotting.jl")

    using Plots

    plotlyjs()

    inputFile = ARGS[1]
    outputFile = ARGS[2]

    bounds = readBounds(inputFile)
    p = plot(
        legend=false,
        size=(600, 400),
    )
    plotBounds!(p, bounds;
        density=true,
        fillcolor=:green4,
        fillalpha=0.7,
        linecolor=:green4,
        linealpha=1.0,
        linewidth=1.0,
    )
    savefig(p, outputFile)
    println("Done.")
end
