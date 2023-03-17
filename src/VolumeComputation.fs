(****************************************************************************************)
(*                                                                                      *)
(*                                      VolumeComputation.fs                            *)
(*                                                                                      *)
(****************************************************************************************)
(*                                                                                      *)
(* A F# interface to use vinci                                                          *)
(*                                                                                      *)
(****************************************************************************************)
module VolumeComputation

open Interval
open Util
open Util.SystemCallUtil
open LinearFunction
open UnionFind

open System
open System.Collections.Concurrent

/// Given the bounds for each sample variable, this method outputs a string in the VINCI format describing the resulting polytope
/// See the VINCI documentation for an Overview of the format
let private generateSimpleBounds (maxVar: int) (bounds: VarBoundMap) =
    let mutable output = ""
    let mutable count = 0

    for (i, iv) in Map.toSeq bounds do
        if Double.IsFinite iv.lo then
            output <- output + $" {-iv.lo}"

            for j in 0 .. maxVar do
                output <- output + if i = j then " -1" else " 0"

            output <- output + "\n"
            count <- count + 1

        if Double.IsFinite iv.hi then
            output <- output + $" {iv.hi}"

            for j in 0 .. maxVar do
                output <- output + if i = j then " 1" else " 0"

            output <- output + "\n"
            count <- count + 1

    output, count



/// Given a list of LinearInequalities without any variables, check if all of them hold
let private allInequalitiesHold (ineConst: list<LinearInequality>) =
    List.forall
        (fun { LinearInequality.Function = { Coefficients = coef; Offset = abs }
               Com = com
               Threshold = thres } ->
            Map.isEmpty coef
            && (match com with
                | LEQ -> abs <= thres
                | LT -> abs < thres
                | GT -> abs > thres
                | GEQ -> abs >= thres))
        ineConst

/// Given a list of LinearInequalities and bounds on the variables, this function encodes the polytope as a string in the VINCI format
let generateVinciInput (conditions: list<LinearInequality>) (split: VarBoundMap) =
    // Compute all variables used
    let usedVars =
        List.map (fun (x: LinearInequality) -> x.UsedVars) conditions
        |> Set.unionMany

    // Generate a variable mapping that maps all used variables to a set {0, ..., n}
    let mutable varMap = Map.empty
    let mutable boundsMap = Map.empty
    let mutable numVars = 0

    for i in usedVars do
        varMap <- Map.add numVars i varMap
        boundsMap <- Map.add numVars split.[i] boundsMap
        numVars <- numVars + 1

    // Generate the string for the bounds in split
    let varBounds, lineCount =
        generateSimpleBounds (numVars - 1) boundsMap

    // Print the standard block
    let header: string =
        sprintf "%i" (conditions.Length + lineCount)
        + " "
        + sprintf "%i" (usedVars.Count + 1)
        + "\n"

    let mutable output = header + varBounds

    // Print every inequality
    for { Function = { Coefficients = coef; Offset = abs }
          Com = com
          Threshold = thres } in conditions do
        match com with
        | LEQ
        | LT ->
            output <- output + $" {-(abs - thres)}"

            for i in 0 .. (numVars - 1) do
                let orVar = Map.find i varMap

                output <-
                    output
                    + " "
                    + (match Map.tryFind orVar coef with
                       | Some x -> $"{x}"
                       | None -> "0")
        | GT
        | GEQ ->
            output <- output + " " + sprintf $"{abs - thres}"

            for i in 0 .. (numVars - 1) do
                let orVar = Map.find i varMap

                output <-
                    output
                    + " "
                    + (match Map.tryFind orVar coef with
                       | Some x -> $"{-x}"
                       | None -> "0")

        output <- output + "\n"

    output

/// If all linear inequalities use only a single variables (have the form a x < b) we can compute the volume more efficiently and not resort to VINCI.
/// In this case, the polytope is just a multi-dimensional box.
/// This is often the case for programs that use a continuous sample to implement a discrete sample
let computeVolumeBox (conditions: list<LinearInequality>) (split: VarBoundMap) =
    let isSingleVar (x: LinearFunction) = Set.count x.UsedVars = 1

    if List.forall (fun (x: LinearInequality) -> isSingleVar x.Function) conditions then
        // All conditions are single Var, we can solve this directly

        // For each variables, we maintain upper and lower bounds (initially this is the bound given by split)
        // We need to consider ALL variables, even if they are not used in any condition in conditions as this still impact the volume
        // For example, if we have the constraint 0 <= x <= 1 and the bounds (given by split) of 0 <= x <= 1, and 0 <= y <= 2, we want to compute volume=2
        // If we only consider those variables that are used in some conditions it would give volume=1 which results in wrong bounds in the further computation (as we multiply the volume with pdf-bounds on the split area)
        let mutable bounds = split

        // Iterate over every Linear Inequality
        for cond in conditions do
            let var =
                (cond.Function.UsedVars |> Set.toList).[0] // By assumption there is exactly one variable

            let factor = cond.Function.Coefficients.[var]
            let functionOffset = cond.Function.Offset
            let threshold = cond.Threshold
            // The linear function has the form "factor * var + functionOffset >< thresholds"

            if factor = 0.0 then 
                // The weight is zero, so either this constraints always holds or it does not 
                let isSat = 
                    match cond.Com with 
                    | LEQ -> functionOffset <= threshold
                    | LT -> functionOffset < threshold
                    | GEQ -> functionOffset >= threshold
                    | GT -> functionOffset > threshold

                if isSat then 
                    ()
                else 
                    // This constraint is unsat, so we set the var-dimension to zero (effectley, setting the computd volume to 0)
                    bounds <- Map.add var (preciseInterval 0.0 0.0) bounds
            else 
                let t = (threshold - functionOffset) / factor // We can assume factor <> 0.0

                let lower, upper = bounds.[var].ToPair

                if (factor > 0.0 && (cond.Com = LEQ || cond.Com = LT)) || (factor < 0.0 && (cond.Com = GEQ || cond.Com = GT)) then
                    // cond is equivalent to var <= t, so we update the upper bound
                    let newUpper = min upper t
                    bounds <- Map.add var (preciseInterval lower newUpper) bounds
                else
                    // cond is equivalent to var >= t, so we update the lowerBound
                    let newLower = max lower t
                    bounds <- Map.add var (preciseInterval newLower upper) bounds


        // bounds now contains the bounds on all variables (recall that the polytope is a rectangle)
        // We can compute the volume by multiplying the side lengths.
        let vol =
            bounds
            |> Map.map (fun _ i -> i.Diam())
            |> Map.fold (fun s _ i -> s * i) 1.0

        Some vol
    else
        // No direction solution is possible
        None

let vinciStopwatch = System.Diagnostics.Stopwatch()
let lpSolveStopwatch = System.Diagnostics.Stopwatch()

// Computes the volume by calling the external tool vinci and parsing its output
let computeVolumeVinci (s: string) : double =
    // We get the path of the GuBPI executable. By convention, the vinci execuatble is located in the same dirctory
    let vinciPath = 
        System.IO.Path.Join [|System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location); "vinci"|]

    vinciStopwatch.Start()
    let out = Util.SystemCallUtil.systemCall vinciPath ("\"" + s + "\"") None
    vinciStopwatch.Stop()

    match out with 
    | SystemCallOutcome out -> 
        if out.Contains "unbounded!" then
            System.Double.PositiveInfinity
        else
            double (out)

    | SystemCallTimeout ->
        printfn "Vinci timed out"
        exit 0
            
    | SystemCallError err -> 
        printfn "An error occured while performing analysis via vinci."
        printfn $"The input was %s{s}\n"
        printfn $"The error was:\n%A{err}"
        exit 0


// A map used to hash the results of the computation
let mutable hashedRes = Map.empty

// A concurrent map in case we wish to run GuBPI in mulithreaded mode
let hashedResConc =
    new ConcurrentDictionary<list<LinearInequality> * VarBoundMap, float>()

let computeVolumeDirectly (conditions: list<LinearInequality>) (split: VarBoundMap) =

    if GlobalConstants.hashVolumeResults
       && (not GlobalConstants.runParallel)
       && Map.containsKey (conditions, split) hashedRes then
        // We run in sequential mode and the result is hashed (in hashedRes)
        hashedRes.[(conditions, split)]
    elif
        GlobalConstants.hashVolumeResults
        && GlobalConstants.runParallel
        && hashedResConc.ContainsKey(conditions, split)
    then
        // We run in multi-core mode and the result is hashed (in hashedResConc)
        hashedResConc.[(conditions, split)]
    else
        // We first try if we can solve via computeVolumeBox, i.e., the polytope is a box
        let res =
            match computeVolumeBox conditions split with
            | Some res -> res
            | None ->
                // The polytope cannot be solved by computeVolumeBox
                let usedVars =
                    List.map (fun (le: LinearInequality) -> le.UsedVars) conditions
                    |> Set.unionMany


                if Set.isEmpty usedVars then
                    // The polytope uses no vars, so we just check if all varFreeInequlaties hold via allInequalitiesHold
                    if allInequalitiesHold conditions then
                        1.0
                    else
                        0.0
                else
                    // Otherwise we generate the output for VINCI and call Vinci
                    generateVinciInput conditions split
                    |> computeVolumeVinci

        // Add the result to the hasher
        if GlobalConstants.hashVolumeResults then
            if GlobalConstants.runParallel then
                hashedResConc.[(conditions, split)] <- res
            else
                hashedRes <- Map.add (conditions, split) res hashedRes

        res