using Plots
using StatsPlots

function parseInterval(input::AbstractString)::Tuple{Float64, Float64}
    parts = [parse(Float64, strip(p, ['[', ' ', ']'])) for p in split(input, ',')]
    tuple(parts...)
end

function readBounds(file::String)::Array
    lines = readlines(file)
    bounds = []
    for line in lines
        parts = split(line, ':')
        if length(parts) != 2
            continue
        end
        if ',' ∉ parts[1]
            # println("$(parts[1]): $(parts[2])")
            continue
        end
        xIv = parseInterval(parts[1])
        yIv = parseInterval(parts[2])
        append!(bounds, [tuple(xIv, yIv)])
    end
    bounds
end

function readSamples(file::String)::Array
    [parse(Float64, s) for s in readlines(file)]
end

function boundToRectangle(bound; density)::Shape
    (xlo, xhi), (ylo, yhi) = bound
    if density
        ylo /= xhi - xlo
        yhi /= xhi - xlo
    end
    Shape([(xlo, ylo), (xlo, yhi), (xhi, yhi), (xhi, ylo)])
end

function plotBounds!(p, bounds; density=false, kwargs...)
    rectangles = [boundToRectangle(b; density=density) for b in bounds]
    plot!(p, rectangles; linecolor=:grey, linealpha = 1, fillcolor=:grey, fillalpha=0.1, label="bounds", kwargs...)
end

function samplesToHistogram(bounds, samples; density=false)::Vector{Tuple{Float64,Float64,Float64}}
    bins = []
    total = length(samples)
    for ((xlo, xhi), _) in bounds
        count = 0
        for sample in samples
            if xlo <= sample && sample <= xhi
                count += 1
            end
        end
        y = count / total
        if density
            y /= xhi - xlo
        end
        append!(bins, [(xlo, xhi, y)])
    end
    bins
end

function histogramLine(bin)::Shape
    xlo, xhi, y = bin
    Shape([(xlo, y), (xhi, y)])
end

function histogramBar(bin)::Shape
    xlo, xhi, y = bin
    Shape([(xlo, 0), (xlo, y), (xhi, y), (xhi, 0)])
end

function plotSamplesHistogramLines!(p, bounds, samples; density, kwargs...)
    histogram = samplesToHistogram(bounds, samples; density=density)
    lines = [histogramLine(bin) for bin in histogram]
    plot!(p, lines; kwargs...)
end

function plotSamplesHistogramBars!(p, bounds, samples; density, kwargs...)
    histogram = samplesToHistogram(bounds, samples; density=density)
    bars = [histogramBar(bin) for bin in histogram]
    plot!(p, bars; fillalpha=0.1, kwargs...)
end

