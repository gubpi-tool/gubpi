(****************************************************************************************)
(*                                                                                      *)
(*                                      Analysis.fs                                     *)
(*                                                                                      *)
(****************************************************************************************)
(*                                                                                      *)
(* Starting Point of the Analysis of a Symbolic Path. Determines if the interval        *)
(* analysis of the optimized analysis (via Volume computation) should be used           *)
(*                                                                                      *)
(****************************************************************************************)

module Analysis

open System

open Interval
open Weight
open Evaluation
open Util
open Expression

/// Analyze a symbolic program path via either the pure Interval-based semantics or optimized volume computation
let private computeHistogramForPath (p: ProgramPath) config =
    let guards = p.Guards

    if config.method = Boxes || List.exists (fun (x: Guard) -> x.Value.TryToIntervalLinearFunction().IsNone) guards || p.Value.TryToIntervalLinearFunction().IsNone then
        // The guard is not linear or box treatment is forced -> need to analyse via Interval based semantics

        let histogram, outside =
            AnalysePathBox.computeHistogramForPath p config

        histogram, outside, Weight.Zero
    else
        // All guards are linear, so we can use the optimized analysis using volume computation
        let dd = config.discretization

        let data =
            Array.create dd.NumberOfBins WeightInterval.Zero

        // We handle each discretized area separately
        for i in 0 .. dd.NumberOfBins - 1 do
            let (a, b) =
                dd.Start + double (i) * dd.StepSize, min (dd.Start + double (i + 1) * dd.StepSize) dd.End

            if GlobalConstants.outputCurrentArea then
                printfn $"Working on area [%f{a}, %f{b}]"

            let b =
                AnalysePathVolume.computeBoundsForResultRange
                    p
                    (preciseInterval a b)
                    config.epsilonScore
                    config.epsilonVar

            Array.set data i b

        // Compute the bounds left of the discretized area
        let outL =
            AnalysePathVolume.computeBoundsForResultRange
                p
                (Interval(Double.NegativeInfinity, dd.Start))
                config.epsilonScore
                config.epsilonVar

        // Compute the bounds right of the discretized area
        let outR =
            AnalysePathVolume.computeBoundsForResultRange
                p
                (Interval(dd.End, Double.PositiveInfinity))
                config.epsilonScore
                config.epsilonVar

        data, outL + outR, Weight.Zero


/// Analyse a path fragment via either the pure Interval-based semantics or optimized volume computation
let private analysePathFragment (p: PathFragment) config =
    let guards = p.Guards

    if config.method = Boxes || List.exists (fun (x: Guard) -> x.Value.TryToIntervalLinearFunction().IsNone) guards then
        // If there exists a guard which is non-linear or boxes is forced, we resort to the pure interval-based analysis
        AnalysePathBox.computeBoundsForFragment p config, Weight.Zero
    else
        // If all guards are linear, use the optimized computation instead. In this case, the unexplored volume is 0
        AnalysePathVolume.computeBoundsForFragment p (config.epsilonScore) (config.epsilonVar), Weight.Zero


/// Infers static bounds on the weight of a path by multiply the bounds on each symbolic score
let private staticBoundsOnPathFragmentScore (pf: PathFragment) =
    let varBounds =
        Map.map (fun _ (d: SymbolicDistribution) -> d.Bounds) pf.SampleDistributions

    List.fold
        (fun s (x: SymbolicValue) -> s * (toWeightInterval (x.EvalIntervalMap varBounds).ExtractInterval))
        WeightInterval.One
        pf.ScoreValues

/// The starting Point of the Analysis of a Symbolic Path.
/// Given a path and configuration, the function computes bounds on the denotation of this path.
let computeHistogram (p: ProgramPath) (config: Hyperparameters) : WeightInterval [] * WeightInterval =

    match Evaluation.cleanUpPath p with
    | None ->
        // This path is infeasible, i.e., has probability 0
        let bounds =
            Array.create config.discretization.NumberOfBins WeightInterval.Zero
        bounds, WeightInterval.Zero
    | Some p ->
        // We split this path into path fragments where the samples are not related
        let mainPath, pathFragments =
            Evaluation.extractIndependentPathFragments p

        // Compute the bounds on the main path (that determines the final value along the path)
        let bs, out, unexploredVol = computeHistogramForPath mainPath config

        let unexplored =
            if unexploredVol.isZero() then
                Weight.Zero
            else
                // If the analysis did not explore the entire path (unexploredVol <> 0.0), we have to account for the missing mass
                // We infer static bounds on the weight of the path and multiple with the unexplored volume
                unexploredVol
                * (staticBoundsOnPathFragmentScore mainPath.Fragment)
                    .hi

        // Add the missing mass to each of the upper bounds
        let adjustedBs =
            Array.map (fun x -> x + weightUpTo unexplored) bs


        // Analyse path fragments which are only relevant for the weight of the path (the return value is determined by mainPath)
        let mutable b = WeightInterval.One

        for p in pathFragments do
            let local, unexploredVol =
                analysePathFragment p config

            let unexplored =
                if unexploredVol.isZero() then
                    Weight.Zero
                else
                    unexploredVol
                    * (staticBoundsOnPathFragmentScore p).hi

            // Add the missing mass to the upper bound
            b <- b * (local + weightUpTo unexplored)


        // Scale the bounds on each bucket from mainPath by the bounds of the path fragments
        Array.map (fun iv -> iv * b) adjustedBs, out * b
