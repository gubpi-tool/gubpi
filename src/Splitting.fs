(****************************************************************************************)
(*                                                                                      *)
(*                                      Splitting.fs                                    *)
(*                                                                                      *)
(****************************************************************************************)
(*                                                                                      *)
(* Helper Functions to Split Linear Functions and Samples                               *)
(*                                                                                      *)
(****************************************************************************************)
module Splitting

open System

open Interval
open Util
open LinearFunction
open PrimitiveFunctions
open PrimitiveDistributions
open Expression
open LinearOptimization

/// Computes bounds on the area of the pdf of the symbolic distribution dist
/// Assume that all parameters of dist are linear. This must be ensured by the called
let boundPdf (area: Interval) (dist: SymbolicDistribution) (varBounds: VarBoundMap) =
    // Compute bounds on the range of each parameter of dist
    let argBounds =
        dist.Parameters
        |> List.map (fun (x: SymbolicValue) ->
            LinearOptimization.computeBounds List.empty varBounds (x.ToLinearFunction()))

    // Computes the bounds by plugging in the bounds on the parameters
    let bounds =
        (PrimitiveDistributions.lookupDistributionSymbol dist.Type)
            .BoundsOnPdf
            argBounds
            area

    bounds

/// Computes the bounds of the PDF on a given Split of the function
let computePdfBounds (domainMap: VarDistMap) (splits: VarBoundMap) =
    let mutable pdfBound = Interval.One

    for (v, d) in Map.toSeq domainMap do
        let paramBounds =
            List.map (fun (y: SymbolicValue) -> (y.EvalIntervalMap splits).ExtractInterval) d.Parameters

        let range = splits.[v]

        let b =
            d.GetPrimitiveDistribution().BoundsOnPdf paramBounds range

        pdfBound <- pdfBound * b

    pdfBound

/// Computes bounds on how to split each sample variable used in AnalysePathVolume.fs
/// returns a list of boxes on the sample variables (a Map<int, Interval>)
let splitVarsViaDAG (domainMap: VarDistMap) (epsilonVar: double) =
    
    // As the variable may depend on each other, we compute a graph that summarizes the dependencies
    
    // For each sampleVar point to those which depend on the var
    let mutable forwardEdges = Map.map (fun _ _ -> Set.empty) domainMap

    // For each sampleVar point to those that are needed
    let mutable backwardEdges = Map.empty

    for (k, v) in Map.toSeq domainMap do
        for var in v.UsedVars do
            forwardEdges <- Map.add var (Set.add k forwardEdges.[var]) forwardEdges

        backwardEdges <- Map.add k v.UsedVars backwardEdges

    // This is the main map that we maintain
    let mutable splitsPerNode: Map<int, list<VarBoundMap>> =
        Map.map (fun _ _ -> List.empty) domainMap

    // Compute a order of the sample variables in which to resolve them
    let order = SCC.computeTopoOrder forwardEdges

    // Iterate over the vars in that order. This ensures that vars that the distribution depends on are already split
    for var in order do
        let dist = domainMap.[var]

        // Check if this variable is used later. This is important to avoid splits on uniform distributions
        let furtherDependece = not forwardEdges.[var].IsEmpty

        let dependencies = backwardEdges.[var] |> Set.toList

        // Compute all possible combinations of splits of the previous sample variables
        let combinedSplits =
            dependencies
            |> List.map (fun x -> splitsPerNode.[x] |> Seq.toList)
            |> fun x ->
                if x.IsEmpty then
                    Seq.singleton Map.empty
                else
                    Util.mergeLLMap x

        // For each split of the variables it depends on split.
        // This uses the DetermineSplitBounds of the primitive distribution
        let newSplits =
            [ for existingSplit in combinedSplits do
                  let parameterBounds =
                      dist.Parameters
                      |> List.map (fun x -> (x.EvalIntervalMap existingSplit).ExtractInterval)

                  let splitsForVar =
                      dist
                          .GetPrimitiveDistribution()
                          .DetermineSplitBounds
                          parameterBounds
                          epsilonVar
                          furtherDependece

                  for interv in splitsForVar do
                      Map.add var interv existingSplit ]

        splitsPerNode <- Map.add var newSplits splitsPerNode

    // Now all splits for all variables are listed in splitsPerNode
    // We construct the product, i.e., all possible combination of all possible splits
    
    let finalSplits =
        forwardEdges
        |> Map.filter (fun _ x -> x.IsEmpty)
        |> Map.toList
        |> List.map fst
        |> List.map (fun x -> splitsPerNode.[x] |> Seq.toList)
        |> Util.mergeLLMap

    finalSplits


/// Computes a set of splits for each linear Function.
/// The boolean flag in the returned value indicates, whether this split is actually needed (see Analysis.fs)
let splitLinearFunction
    (conditions: List<LinearInequality>)
    (split: VarBoundMap)
    (linearFunctions: Set<LinearFunction>)
    (epsilonScore: double)
    =
    // Order the set in any way
    let asList = Set.toList linearFunctions

    let mutable combinations = []

    for lf in asList do
        // Use Linear optimization to compute upper and lower bounds on this linear functions. This might be unbounded

        // There is a bug in the Flips library that causes it to say the model is infeasible, even if it is not.
        // In fact, whenever we are at this points, we can assume that the model is feasible (ensured by the caller), so whenever NONE is returned we just map it to +- infty
        
        let lowerBound =
            match LinearOptimization.optimize conditions split lf MIN true with
            | Some res -> res
            | None -> Double.NegativeInfinity

        let upperBound =
            match LinearOptimization.optimize conditions split lf MAX true with
            | Some res -> res
            | None -> Double.PositiveInfinity

        // Now compute how to split the range [lowerBound, upperBound] into smaller segments. 
        let intervals =
            if Double.IsFinite lowerBound
               && Double.IsFinite upperBound then
                // Both bounds are finite, so we can actually split this into epsilonScore-big chunks. First determine the number of splits needed
                let numberOfSplits =
                    max
                        ((upperBound - lowerBound) / epsilonScore
                         |> Math.Ceiling
                         |> int)
                        1
                        
                // Afterwards we can compute the actual splitting points which are of size epsilonScore
                let splittingPointsForPurePart =
                    [ for i in 0 .. numberOfSplits do
                          min (lowerBound + double i * epsilonScore) upperBound ]

                // The split is important if there is more than 1 split of this function
                let isImportantSplit = numberOfSplits > 1

                // return all the intervals between two splitting points
                [ for j in 0 .. splittingPointsForPurePart.Length - 2 do
                      preciseInterval splittingPointsForPurePart.[j] splittingPointsForPurePart.[j + 1],
                      isImportantSplit ]
            else
                // One of the bounds is infinite, so we do not split but just return the bounds
                // This could be improved by some heuristic
                [ preciseInterval lowerBound upperBound, false ]

        combinations <- intervals :: combinations

    // After having computed the splitting points for all variables we take the combination of al such splits. 
    let res =
        combinations
        |> List.rev
        |> Util.cart1
        |> Seq.map (fun x -> List.zip asList x |> Map.ofList)

    res
