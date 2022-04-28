(****************************************************************************************)
(*                                                                                      *)
(*                                      Plotting.fs                                     *)
(*                                                                                      *)
(****************************************************************************************)
(*                                                                                      *)
(* Basic Plotting of Denotation Bounds using XPlot.Plotly                               *)
(*                                                                                      *)
(****************************************************************************************)

module Plotting

open XPlot.Plotly


type DenType = NORM | UNNORM


/// Plots Lower and Upper Bounds on the Denotation via XPlot.Plotly.
let plotTwo (t : DenType) (xPoints: array<double>) (dataLower: array<double>) (dataUpper: array<double>) file =
    let traceLower =
        Scatter(x = xPoints, y = dataLower, mode = "markers", name = "lower bound")

    let traceUpper =
        Scatter(x = xPoints, y = dataUpper, mode = "markers", name = "upper bound")

    let styledLayout =
        Layout(
            title = (match t with NORM -> "Normalized" | UNNORM -> "Unormalized") +  " Denotation via Symbolic and Static Analysis",
            xaxis = Xaxis(title = "Range"),
            yaxis = Yaxis(title = "Normalized Denotation", showline = false)
        )

    let chart =
        [ traceLower; traceUpper ]
        |> Chart.Plot
        |> Chart.WithLayout styledLayout
        |> Chart.WithWidth 700
        |> Chart.WithHeight 500

    let chartHtml = chart.GetHtml()
    System.IO.File.WriteAllText(file, chartHtml)
