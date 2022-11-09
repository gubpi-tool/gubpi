(****************************************************************************************)
(*                                                                                      *)
(*                                      AnalysePathVolume.fs                            *)
(*                                                                                      *)
(****************************************************************************************)
(*                                                                                      *)
(* Starting Point of the Analysis using the optimized semantics using Volume Computation*)
(*                                                                                      *)
(****************************************************************************************)

module AnalysePathVolume

open System

open Interval
open Weight
open Util
open PrimitiveDistributions
open LinearFunction
open Expression
open Evaluation
open ScoreClosure
open Splitting

/// Approximates the integral using successive Volume computations if the guards to not contain any intervals
let private approximateIntegral
    (conditions: list<LinearInequality>)
    (varMap: VarDistMap)
    (scoreValues: list<SymbolicValue>)
    (epsilonScore: double)
    (epsilonVar: double)
    =

    // Convert all symbolic score functions to score closures by identifying large linear fragments within them
    let scoreClosures = List.map toScoreClosure scoreValues

    // Extract all linear Functions used within the scores to avoid duplicates
    let linearFunctions =
        scoreClosures
        |> List.collect (fun x -> x.LinearParts)
        |> set

    let mutable bound = WeightInterval.Zero

    // Split the sample variables if needed.
    // Splitting.splitVarsViaDAG returns a list of boxes (splits) that discretize the sample outcome
    // We filter for those boxes/splits that are feasible.
    let splits =
        Splitting.splitVarsViaDAG varMap epsilonVar
        |> Seq.filter (fun x -> LinearOptimization.isFeasible conditions x)
        |> Seq.toList


    let mutable currentVarSplit = 0

    let mutable oldVarSplitPer = -1
    let mutable oldScoreSplitPer = -1

    // We iterate over every possible split of the sample variables.
    // If all samples are from the uniform distribution, there is only one split, i.e., splits has length 1.
    for split in splits do
        currentVarSplit <- currentVarSplit + 1

        // Based on the current splits, compute the bounds on the pdf of the distribution each variable is sampled from
        let pdf = computePdfBounds varMap split |> toWeightInterval

        // Split each of the linear parts appearing in the score closures (each element in linearFunctions) into smaller blocks to approximate the integral
        let boundsForLinearFunctions =
            splitLinearFunction conditions split linearFunctions epsilonScore
            |> Seq.toList

        let mutable currentScoreSplit = 0


        for bounds in boundsForLinearFunctions do
            currentScoreSplit <- currentScoreSplit + 1

            // --------------------- Handle Prints ----------------------
            if GlobalConstants.outputSplitProgress then
                // Avoid to many printouts as this slows things down and only print the percentage.
                let varSplitPer = (currentVarSplit * 100) / splits.Length

                let scoreSplitPer =
                    (currentScoreSplit * 100)
                    / boundsForLinearFunctions.Length

                if varSplitPer <> oldVarSplitPer
                   || scoreSplitPer <> oldScoreSplitPer then
                    // Only print when changes occur
                    System.Console.SetCursorPosition(0, System.Console.CursorTop)

                    oldVarSplitPer <- varSplitPer
                    oldScoreSplitPer <- scoreSplitPer

                    printf
                        "%i VarSplits. %i %%done.  %i ScoreSplits.  %i %%done"
                        splits.Length
                        varSplitPer
                        boundsForLinearFunctions.Length
                        scoreSplitPer
            // --------------------- End - Handle Prints ----------------------

            let mutable guards = conditions

            let mutable scoreFactorGuard = WeightInterval.One

            // As each linear part appearing in the score closure is bounded by an interval in bounds, we compute bounds on the value of the score within the current bounds.
            for sc in scoreClosures do
                let w =
                    sc.LinearParts
                    |> List.map (fun x -> Map.find x bounds |> fst)
                    |> sc.BoundNonLinearPart

                scoreFactorGuard <- scoreFactorGuard * toWeightInterval w

            for (lf, (iv, isImportantSplit)) in Map.toSeq bounds do
                // Add the bounds on each linear function to the current polytope (only if they are finite)
                // isImportantSplit holds if the split on the linear function is a proper split.
                // For example, if the liner function lf is always bounded by [a, b], then the split [a, b] is not proper and so we do not need to add this to the polytope.

                if isImportantSplit then
                    // Only add if this is an important split, i.e., actually splits and is not just the obvious bounds
                    guards <-
                        if Double.IsFinite iv.lo then
                            [ { Function = lf
                                Com = GEQ
                                Threshold = iv.lo } ]
                        else
                            []
                        @ if Double.IsFinite iv.hi then
                              [ { Function = lf
                                  Com = LEQ
                                  Threshold = iv.hi } ]
                          else
                              []
                          @ guards

            // Before computing the volume using VINCI, we check if the polytope is non-empty. This is mich cheaper to compute
            if LinearOptimization.isFeasible guards split then

                // Compute the volume using VINCI
                let localVol =
                    VolumeComputation.computeVolumeDirectly guards split

                let boundCdf (v, iv: Interval) =
                    let symDist = varMap.[v]
                    let dist = lookupDistributionSymbol symDist.Type

                    let paramBounds =
                        List.map
                            (fun (y: SymbolicValue) -> (y.EvalIntervalMap split).ExtractInterval)
                            symDist.Parameters

                    let lo = dist.BoundsOnCdf paramBounds iv.lo
                    let hi = dist.BoundsOnCdf paramBounds iv.hi
                    hi - lo |> ensureNonnegative

                let local =
                    if Double.IsInfinity localVol then
                        // If the volume is infinite, we can still obtain a finite bounds by computing the cdf (opposed to pdf) of each of the sampled varaibles
                        // If e.g., sample variable \alpha_i is sampled from a normal and bound to [-infty, 0] in split, we can infer the cdf(0)-cdf(infty) to still obtain sound bounds on the volume
                        // Even though the volume of the polytope might be unbounded
                        // In most cases (in particular if all samples are from a uniform), this case never occurs
                        let cdfBounds = split |> Map.toSeq |> Seq.map boundCdf |> Seq.map toWeightInterval
                        let cdfBoxBounds = Seq.fold (*) WeightInterval.One cdfBounds

                        cdfBoxBounds * scoreFactorGuard
                    else
                        // If the volume is finite, we just multiple the volume times the bounds on score values and the pdfs of the sampled values
                        preciseWeight (toWeight localVol) * scoreFactorGuard * pdf

                bound <- bound + local

    if GlobalConstants.outputSplitProgress then
        printf "\n"

    bound

/// Computes Bounds on the integral over the area spanned by conditions (the distribution given by varMap) and the function given as the product of the scoreValues.
let private approximateIntegralPlus
    (conditions: list<IntervalLinearInequality>)
    (varMap: VarDistMap)
    (scoreValues: list<SymbolicValue>)
    (epsilonScore: double)
    (epsilonVar: double)
    =
    // The conditions can contain inequalities that contain intervals, so we cannot compute the integral directly.
    // We replace such interval bounds using under- and overapproximations

    /// Removes the Interval from a Linear Inequality by computing a inequality that describes a smaller solution
    let computeNesessaryApprox (g: IntervalLinearInequality) : LinearInequality =
        let lo, hi = g.Function.AddedInterval.ToPair
        let f = g.Function.Linear

        let newF =
            match g.Com with
            | LEQ
            | LT ->
                // c * x + [lo, hi] < t -> c * x + lo < t
                { g.Function.Linear with Offset = f.Offset + lo }
            | GEQ
            | GT ->
                // c * x + [lo, hi] > t -> c * x + hi > t
                { g.Function.Linear with Offset = f.Offset + hi }

        { LinearInequality.Function = newF
          Com = g.Com
          Threshold = g.Threshold }

    /// Removes the Interval from a Linear Inequality by computing a inequality that describes a larger solution
    let computeSufficentApprox (g: IntervalLinearInequality) : LinearInequality =
        let lo, hi = g.Function.AddedInterval.ToPair
        let f = g.Function.Linear

        let newF =
            match g.Com with
            | LEQ
            | LT ->
                // c * x + [lo, hi] < t <- c * x + hi < t
                { g.Function.Linear with Offset = f.Offset + hi }
            | GEQ
            | GT ->
                // c * x + [lo, hi] > t <- c * x + lo > t
                { g.Function.Linear with Offset = f.Offset + lo }

        { LinearInequality.Function = newF
          Com = g.Com
          Threshold = g.Threshold }

    // Compute bounds on each sampled variable
    let bounds = varMap |> Map.map (fun _ x -> x.Bounds)

    /// Checks if the Guard is definitely unsat, i.e., the entire program path containing this path is infeasible
    let isGuardAlwaysUnsat (g: LinearInequality) =
        let v = g.Function.EvalWithGivenBounds bounds

        match g.Com with
        | LEQ -> v.lo > g.Threshold
        | LT -> v.lo >= g.Threshold
        | GEQ -> v.hi < g.Threshold
        | GT -> v.hi <= g.Threshold

    /// Checks if the Guard is always sat, i.e., it can be eliminated from the program path
    let isGuardAlwaysSat (g: LinearInequality) =
        let v = g.Function.EvalWithGivenBounds bounds

        match g.Com with
        | LEQ -> v.hi <= g.Threshold
        | LT -> v.hi < g.Threshold
        | GEQ -> v.lo >= g.Threshold
        | GT -> v.lo > g.Threshold

    /// Computes the integral for the given set of Linear Inequality
    let computeRes (l: list<LinearInequality>) : WeightInterval =
        if List.exists isGuardAlwaysUnsat l then
            WeightInterval.Zero
        else
            // Filter out Guards that are always sat
            let guards = List.filter (isGuardAlwaysSat >> not) l

            if List.exists (fun (x: LinearInequality) -> not (x.isFinite)) guards then
                // There exists a linear inequality with a non-finite constant. We can thus only use this as an upper bound and set the lower bound to 0

                let remainingGuards =
                    List.filter (fun (x: LinearInequality) -> x.isFinite) guards

                let res =
                    approximateIntegral remainingGuards varMap scoreValues epsilonScore epsilonVar

                weightUpTo res.hi
            else
                // All constants are finite. Proceed to approximateIntegral.
                let res =
                    approximateIntegral guards varMap scoreValues epsilonScore epsilonVar

                res

    // Compute the larger area of integration
    let largerVolumeConditions =
        List.map computeNesessaryApprox conditions

    // Compute the smaller area of integration
    let smallerVolumeConditions =
        List.map computeSufficentApprox conditions

    if largerVolumeConditions = smallerVolumeConditions then
        // The path contained no intervals. We can just progress as before
        let cond = smallerVolumeConditions
        computeRes cond
    else
        // We compute the lower bound on the smaller area and the upper bound on the larger area.
        let smallerBounds = computeRes smallerVolumeConditions
        let largerBounds = computeRes largerVolumeConditions

        preciseWeightInterval smallerBounds.lo largerBounds.hi


/// Compute lower and upper bounds on the denotation of the symbolic path p to be within range.
/// epsilonScore gives the granularity with which scores are splitted (to approximate the integral)
/// epsilonVar gives the granularity with which non-uniform samples are splitted
let computeBoundsForResultRange (p: ProgramPath) (range: Interval) epsilonScore (epsilonVar: double) =
    // Add two new guards that require the value to be within [a, b]
    // We assume that the Value does not contain any intervals and is linear, this needs to be ensured by the caller (in Analysis.fs)
    let lf = p.Value.ToIntervalLinearFunction()

    let initConditions =
        let a, b = range.ToPair

        (if Double.IsFinite a then
             [ { Function = lf
                 Com = GEQ
                 Threshold = a } ]
         else
             [])
        @ (if Double.IsFinite b then
               [ { Function = lf
                   Com = LEQ
                   Threshold = b } ]
           else
               [])
          @ List.map (fun (g: Guard) -> g.ToIntervalLinearInequality()) p.Guards

    // After we added the guards a <= lf <= b, we approximate the integral over the area spanned by the guards
    let bounds =
        approximateIntegralPlus initConditions p.SampleDistributions p.ScoreValues epsilonScore epsilonVar

    bounds

/// Compute bounds on the integral of a path fragment.
/// Different from computeBoundsForResultRange, no range is given as a path fragment has no return value (only impacts the weight).
let computeBoundsForFragment (p: PathFragment) epsilonScore (epsilonVar: double) =
    let initConditions =
        List.map (fun (g: Guard) -> g.ToIntervalLinearInequality()) p.Guards

    approximateIntegralPlus initConditions p.SampleDistributions p.ScoreValues epsilonScore epsilonVar
