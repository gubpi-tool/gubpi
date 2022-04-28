(****************************************************************************************)
(*                                                                                      *)
(*                                      AnalysePathBox.fs                               *)
(*                                                                                      *)
(****************************************************************************************)
(*                                                                                      *)
(* Analyse the volume using the Interval-based semantics                                *)
(*                                                                                      *)
(****************************************************************************************)

module AnalysePathBox

open System
open FSharpx.Collections

open Interval
open Util
open Expression
open PrimitiveDistributions
open Evaluation

[<Struct>]
type Trilean =
    | True
    | False
    | Unknown
    static member inline (&&&)(c1: Trilean, c2: Trilean) =
        match c1 with
        | True -> c2
        | False -> False
        | Unknown ->
            match c2 with
            | False -> False
            | _ -> Unknown


type FragmentResult =
    { mutable NormConst: Interval }

type PathResult =
    { mutable NormConst: Interval
      mutable Expectation: Interval
      mutable Histogram: Interval []
      mutable Outside: Interval }

/// Checks if a given guard is satfied on a box
let private satisfiesGuard (b: Box) (g: Guard) =
    let (a, b) =
        (g.Value.EvalBox b).ExtractInterval.ToPair

    match g.Com with
    | LEQ ->
        if b <= g.Threshold then True
        elif a > g.Threshold then False
        else Unknown
    | LT ->
        if b < g.Threshold then True
        elif a >= g.Threshold then False
        else Unknown
    | GT ->
        if a > g.Threshold then True
        elif b <= g.Threshold then False
        else Unknown
    | GEQ ->
        if a >= g.Threshold then True
        elif b < g.Threshold then False
        else Unknown

let private satisfiesAllGuards (b: Box) (gs: Guard list) =
    gs
    |> List.fold (fun s g -> s &&& satisfiesGuard b g) True

/// Splits a box in dimension i
let private splitBox (box: Box) (i: int) : Box * Box =
    let (a, b) = box.[i].ToPair

    let aInf = Double.IsInfinity a
    let bInf = Double.IsInfinity b

    let mid =
        if aInf && bInf then
            0.0
        elif aInf then
            if b < 0.0 then 2.0 * b
            elif b > 0.0 then 0.0
            else -1.0
        elif bInf then
            if a > 0.0 then 2.0 * a
            elif a < 0.0 then 0.0
            else 1.0
        else
            (a + b) / 2.0

    let first = Array.copy box
    first.[i] <- preciseInterval a mid

    let second = Array.copy box
    second.[i] <- preciseInterval mid b

    (first, second)

let private logBoxVolume (b: Box) =
    b
    |> Array.fold (fun v iv -> v + Math.Log(iv.Diam())) 0.0

let private sigmoid (x: double) = 1.0 / (1.0 + Math.Exp(-x))

let private logsumexp (xs: double []) =
    let max = Array.reduce max xs

    let sumexp =
        xs
        |> Array.fold (fun s x -> s + Math.Exp(x - max)) 0.0

    max + Math.Log sumexp


[<CustomComparison; Struct;CustomEquality>]
type private BoxResultFragment =
    { box: Box
      priority: double
      logvolume: Interval
      logweight: Interval
      splitDepth: int }

    member this.Integral: Interval = this.LogIntegral.Exp()
    member this.LogIntegral: Interval = this.logweight + this.logvolume
    
    interface IComparable with
        member this.CompareTo other =
            match other with
            | :? BoxResultFragment as other ->
                if this.priority < other.priority then
                    -1
                elif this.priority > other.priority then
                    1
                else
                    0
            | _ -> -1

    override this.GetHashCode() = 0 

    override this.Equals(p) =
        match p with
        | :? BoxResultFragment as b -> 
            this.box = b.box && this.priority = b.priority && this.logvolume = b.logvolume && this.logweight = b.logweight && this.splitDepth = b.splitDepth
        | _ -> false

[<CustomComparison; Struct;CustomEquality>]
type private BoxResultPath =
    { resFragment: BoxResultFragment;
      value: Interval}

    member this.Integral: Interval = this.resFragment.Integral

    member this.box = this.resFragment.box
    member this.priority = this.resFragment.priority
    member this.logvolume = this.resFragment.logvolume
    member this.logweight = this.resFragment.logweight
    member this.splitDepth = this.resFragment.splitDepth

    interface IComparable with
        member this.CompareTo other =
            match other with
            | :? BoxResultPath as other ->
                if this.resFragment.priority < other.resFragment.priority then
                    -1
                elif this.resFragment.priority > other.resFragment.priority then
                    1
                else
                    0
            | _ -> -1

    override this.GetHashCode() = 0 

    override this.Equals(p) =
        match p with
            | :? BoxResultPath as b -> 
                this.resFragment = b.resFragment && this.value = b.value
            | _ -> false
    
/// Computes the (log) volume of a box, i.e. the integral of the density of each sample over the box
let private logVolume (p: PathFragment) (box: Box) =
    let logProbMass (varId: int) (symDist: SymbolicDistribution) =
        let dist = lookupDistributionSymbol symDist.Type

        let paramBounds =
            List.map (fun (y: SymbolicValue) -> (y.EvalBox box).ExtractInterval) symDist.Parameters

        let x = box.[varId]
        let hiCdf = dist.BoundsOnCdf paramBounds x.hi
        let loCdf = dist.BoundsOnCdf paramBounds x.lo
        let logCdfBound = (hiCdf - loCdf |> intersect unitInterval).Log()

        logCdfBound


    let logvol =
        p.SampleDistributions
        |> Map.fold (fun s k v -> s + logProbMass k v) Interval.Zero
    logvol

let private BIG = 1e100

let private makeFinite (x: double) =
    if x > BIG then BIG
    elif x < -BIG then -BIG
    else x

let private makeFiniteIv (iv: Interval) =
    preciseInterval (makeFinite iv.lo) (makeFinite iv.hi)


/// Evaluates a path fragment on a box to obtain the BoxResultFragment
let private evaluateBoxFragment (p: PathFragment) (box: Box) (splitDepth: int) =
    let logvolume = logVolume p box

    let logweight =
        p.ScoreValues
        |> List.fold (fun s v -> s + (v.EvalBox box).ExtractInterval.Log()) Interval.Zero

    let logweight =
        match satisfiesAllGuards box p.Guards with
        | True -> logweight
        | False -> precisely Double.NegativeInfinity
        | Unknown ->
            logweight
            + (preciseInterval Double.NegativeInfinity 0.0)

    let normConst = (logweight + logvolume).Exp()

    let priority =
        min (normConst.Diam()) 1000
        // + min (log (expectation.Diam())) 10
        - double splitDepth

    { box = box
      priority = priority
      logvolume = logvolume
      logweight = logweight
      splitDepth = splitDepth }


/// Evaluates a path on a box to obtain the BoxResultPath
let private evaluateBoxPath (p: ProgramPath) (box: Box) (splitDepth: int) =
    let value = (p.Value.EvalBox box).ExtractInterval

    let resFragment = evaluateBoxFragment p.Fragment box splitDepth

    { 
        resFragment = resFragment;
        value = value 
    }

/// Find the best split of a given box over a program fragment
/// This is the split that minimizes the integral over boxes
let private findBestSplitFragment (p: PathFragment) (boxResult: BoxResultFragment) : int * BoxResultFragment [] =
    let mutable bestDim = 0
    let mutable bestSplit = [||]
    let mutable bestReduction = 0.0

    for i in 0 .. boxResult.box.Length - 1 do
        let box1, box2 = splitBox boxResult.box i

        let box1Result =
            evaluateBoxFragment p box1 (boxResult.splitDepth + 1)

        let box2Result =
            evaluateBoxFragment p box2 (boxResult.splitDepth + 1)

        let expectationBefore =
            makeFiniteIv (boxResult.Integral)

        let expectationAfter =
            makeFiniteIv (
                box1Result.Integral + box2Result.Integral
            )

        let reduction =
            expectationBefore.hi - expectationAfter.hi
            + expectationAfter.lo
            - expectationBefore.lo

        if reduction > bestReduction then
            bestReduction <- reduction
            bestDim <- i
            bestSplit <- [| box1Result; box2Result |]

    if bestReduction = 0.0 then
        let mutable widestDim = 0
        let mutable widestWidth = 0.0

        for i in 0 .. boxResult.box.Length - 1 do
            let iv = boxResult.box.[i]
            let width = sigmoid iv.hi - sigmoid iv.lo

            if width > widestWidth then
                widestWidth <- width
                widestDim <- i

        let box1, box2 = splitBox boxResult.box widestDim

        let box1Result =
            evaluateBoxFragment p box1 (boxResult.splitDepth + 1)

        let box2Result =
            evaluateBoxFragment p box2 (boxResult.splitDepth + 1)

        widestDim, [| box1Result; box2Result |]
    else
        bestDim, bestSplit


/// Find the best split of a given box over a program
/// This is the split that minimizes the integral over boxes weighted by the range of the value of the path
let private findBestSplitPath (p: ProgramPath) (boxResult: BoxResultPath) : int * BoxResultPath [] =
    let mutable bestDim = 0
    let mutable bestSplit = [||]
    let mutable bestReduction = 0.0

    for i in 0 .. boxResult.box.Length - 1 do
        let box1, box2 = splitBox boxResult.box i

        let box1Result =
            evaluateBoxPath p box1 (boxResult.splitDepth + 1)

        let box2Result =
            evaluateBoxPath p box2 (boxResult.splitDepth + 1)

        let expectationBefore =
            makeFiniteIv (boxResult.value * boxResult.Integral)

        let expectationAfter =
            makeFiniteIv (
                box1Result.value * box1Result.Integral
                + box2Result.value * box2Result.Integral
            )

        let reduction =
            expectationBefore.hi - expectationAfter.hi
            + expectationAfter.lo
            - expectationBefore.lo

        if reduction > bestReduction then
            bestReduction <- reduction
            bestDim <- i
            bestSplit <- [| box1Result; box2Result |]

    if bestReduction = 0.0 then
        let mutable widestDim = 0
        let mutable widestWidth = 0.0

        for i in 0 .. boxResult.box.Length - 1 do
            let iv = boxResult.box.[i]
            let width = sigmoid iv.hi - sigmoid iv.lo

            if width > widestWidth then
                widestWidth <- width
                widestDim <- i

        let box1, box2 = splitBox boxResult.box widestDim

        let box1Result =
            evaluateBoxPath p box1 (boxResult.splitDepth + 1)

        let box2Result =
            evaluateBoxPath p box2 (boxResult.splitDepth + 1)

        widestDim, [| box1Result; box2Result |]
    else
        bestDim, bestSplit

/// Normalizes a path fragment to use sample vars in 0...n-1
let private normalizeFragment (p: PathFragment) =
    let usedVars = p.UsedVars
    let mutable replacement = Map.empty
    let mutable varCount = 0
    let mutable newDistMap = Map.empty

    for i in usedVars do
        replacement <- Map.add i varCount replacement
        newDistMap <- Map.add varCount (p.SampleDistributions.[i]) newDistMap
        varCount <- varCount + 1

    let guards =
        List.map (fun (g: Guard) -> { g with Value = g.Value.ReplaceVars replacement }) p.Guards

    let scoreValues =
        List.map (fun (x: SymbolicValue) -> x.ReplaceVars replacement) p.ScoreValues

    {
        PathFragment.Guards = guards
        PathFragment.SampleDistributions = newDistMap
        PathFragment.ScoreValues = scoreValues
    }
   
/// Normalizes a path to use sample vars in 0...n-1
let private normalizePath (p: ProgramPath) =
    let usedVars = p.UsedVars
    let mutable replacement = Map.empty
    let mutable varCount = 0
    let mutable newDistMap = Map.empty

    for i in usedVars do
        replacement <- Map.add i varCount replacement
        newDistMap <- Map.add varCount (p.SampleDistributions.[i]) newDistMap
        varCount <- varCount + 1

    let guards =
        List.map (fun (g: Guard) -> { g with Value = g.Value.ReplaceVars replacement }) p.Guards

    let scoreValues =
        List.map (fun (x: SymbolicValue) -> x.ReplaceVars replacement) p.ScoreValues

    let value = p.Value.ReplaceVars replacement

    { Value = value
      Fragment =
        { Guards = guards
          ScoreValues = scoreValues
          SampleDistributions = newDistMap } }


/// Compute bounds for a program path by repeatedly splitting boxes
let computeHistogramForPath (p: ProgramPath) (config: Hyperparameters) =
    let p = normalizePath p

    let varCount = Set.count p.UsedVars

    let box =
        [| for i in 0 .. varCount - 1 -> p.SampleDistributions.[i].Bounds |]

    let initialBoxResult = evaluateBoxPath p box 0

    let mutable boxes = PriorityQueue.empty true

    let mutable partitions = [|initialBoxResult.box|]

    printfn $"starting with {partitions.Length} interval traces..."

    for box in partitions do
        let boxResult = evaluateBoxPath p box 0
        boxes <- boxes.Insert(boxResult)

    let mutable numSplits = config.splits

    printfn $"{numSplits} splits remaining..."

    while (not boxes.IsEmpty) && numSplits >= 0 do
        if numSplits % 10000 = 0 then
            printfn $"{numSplits} splits remaining..."

        let curbox, tempBoxes = boxes.Pop()
        boxes <- tempBoxes
        if curbox.box.Length = 0 then
            boxes <- boxes.Insert(curbox)
        else
            let _, splits = findBestSplitPath p curbox

            for splitBox in splits do
                boxes <- boxes.Insert(splitBox)

        numSplits <- numSplits - 1

    let mutable result =
        { NormConst = Interval.Zero
          Expectation = Interval.Zero
          Histogram = Array.create config.discretization.NumberOfBins Interval.Zero
          Outside = Interval.Zero }

    printfn $"\nsummarizing results from interval traces..."

    while not boxes.IsEmpty do
        let box, tempBoxes = boxes.Pop()
        boxes <- tempBoxes

        let integral = box.Integral

        result.NormConst <- result.NormConst + integral

        result.Expectation <- result.Expectation + integral * box.value

        let numBins = config.discretization.NumberOfBins

        let loOut =
            box.value.lo < config.discretization.Start

        let loBin =
            int (
                (box.value.lo - config.discretization.Start)
                / config.discretization.StepSize
            )

        let hiOut = box.value.hi > config.discretization.End

        let hiBin =
            numBins
            - 1
            - int (
                (config.discretization.End - box.value.hi)
                / config.discretization.StepSize
            )

        if not loOut && not hiOut then
            if loBin = hiBin then
                result.Histogram.[loBin] <- result.Histogram.[loBin] + precisely integral.lo

        if loOut || hiOut then
            result.Outside <- result.Outside + zeroTo integral.hi

        let loBin = max 0 loBin
        let hiBin = min (numBins - 1) hiBin

        for i in loBin .. hiBin do
            result.Histogram.[i] <- result.Histogram.[i] + zeroTo integral.hi

    printfn $"{result}"

    result.Histogram, result.Outside

/// Compute bounds on the norm constant of a path fragment by repeatedly splitting boxes
let computeBoundsForFragment (p: PathFragment) (config: Hyperparameters) : Interval =
    let p = normalizeFragment p

    let varCount = Set.count p.UsedVars

    let box =
        [| for i in 0 .. varCount - 1 -> p.SampleDistributions.[i].Bounds |]

    let initialBoxResult = evaluateBoxFragment p box 0

    let mutable boxes = PriorityQueue.empty true

    let mutable partitions = [|initialBoxResult.box|]

    printfn $"starting with {partitions.Length} interval traces..."

    for box in partitions do
        let boxResult = evaluateBoxFragment p box 0
        boxes <- boxes.Insert(boxResult)

    let mutable numSplits = config.splits

    printfn $"{numSplits} splits remaining..."

    while (not boxes.IsEmpty) && numSplits >= 0 do
        if numSplits % 10000 = 0 then
            printfn $"{numSplits} splits remaining..."

        let curbox, tempBoxes = boxes.Pop()
        boxes <- tempBoxes
        if curbox.box.Length = 0 then
            boxes <- boxes.Insert(curbox)
        else
            let _, splits = findBestSplitFragment p curbox

            for splitBox in splits do
                boxes <- boxes.Insert(splitBox)

        numSplits <- numSplits - 1

    let mutable result =
        { FragmentResult.NormConst = Interval.Zero }

    printfn $"\nsummarizing results from interval traces..."

    while not boxes.IsEmpty do
        let box, tempBoxes = boxes.Pop()
        boxes <- tempBoxes

        let integral = box.Integral
        result.NormConst <- result.NormConst + integral
        
    printfn $"{result}"

    result.NormConst