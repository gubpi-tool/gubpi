(****************************************************************************************)
(*                                                                                      *)
(*                                      PrimitiveDistributions.fs                       *)
(*                                                                                      *)
(****************************************************************************************)
(*                                                                                      *)
(* The list of primitive distribution supported by GuBPI.                               *)
(*                                                                                      *)
(****************************************************************************************)

module PrimitiveDistributions

open System
open FSharp.Stats.Distributions
open FSharp.Stats.SpecialFunctions

open Interval
open PrimitiveFunctions

type DistributionSymbol =
    | UNIFORM
    | TRUNCNORMAL // Normal that is truncated, i.e., only samples values in [a, b]
    | NORMAL
    | POISSON
    | BETA

/// Type of the representation of a primitive distribution
type PrimitiveDistribution =
    { Name: String

      Symbol: DistributionSymbol

      // Number of Parameters
      Parameters: int

      // Given interval bounds on each parameters, gives an interval bounds on the possible values
      BoundsOnSamples: list<Interval> -> Interval

      // Given interval bounds on each parameters and a interval. Give lower and upper bound on the density of the distribution on that area.
      BoundsOnPdf: list<Interval> -> Interval -> Interval

      // Given interval bounds on each parameters and a value. Give bounds on the CDF
      BoundsOnCdf: list<Interval> -> double -> Interval

      // Given interval bounds on each parameters and epsilonScore, give a sequence of intervals that split the domain of the distribution
      DetermineSplitBounds: list<Interval> -> double -> bool -> seq<Interval> }

// Uniform Distribution
let Uniform =
    { Name = "uniform"
      Symbol = UNIFORM
      Parameters = 2
      BoundsOnSamples =
        function
        | [ l; r ] -> Interval.hull l r
        | _ -> failwith "Non valid number of parameters"

      BoundsOnPdf =
          fun args _ ->
              match args with
              | [ l; r ] -> (Interval.One / (r - l)) |> ensureNonnegative
              | _ -> failwith "Non valid number of parameters"

      BoundsOnCdf =
          fun args x ->
              match args with
              | [ l; r ] ->
                  let res = (precisely x - l) / (r - l)
                  preciseInterval (max res.lo 0.0) (min res.hi 1.0)
              | _ -> failwith "invalid number of parameters"

      DetermineSplitBounds =
          fun paraBounds epsilonVar isUsed ->
              match paraBounds with
              | [ l; r ] ->
                  let lower, upper = l.lo, r.hi

                  if isUsed then
                      let numberOfSplits =
                          Math.Max(
                              (upper - lower) / epsilonVar
                              |> Math.Ceiling
                              |> int,
                              1
                          )

                      let splittingPointsForPurePart =
                          [ for i in 0 .. numberOfSplits do
                                min (lower + double i * epsilonVar) upper ]

                      seq {
                          for j in 0 .. splittingPointsForPurePart.Length - 2 do
                              preciseInterval (splittingPointsForPurePart.[j]) (splittingPointsForPurePart.[j + 1])
                      }
                  else
                      // No need to split this var
                      Seq.singleton (preciseInterval lower upper)

              | _ -> failwith "Wrong number of args" }


// Truncated Normal
let TruncatedNormal =
    { Name = "truncnormal"
      Symbol = TRUNCNORMAL
      Parameters = 4
      BoundsOnSamples =
        function
        | [ _; _; l; r ] -> Interval.hull l r
        | _ -> failwith "Non valid number of parameters"
      BoundsOnPdf =
        fun args area ->
            match args with
            | [ mean; sigma; _; _ ] -> pdfNormalBounds mean sigma area
            | _ -> failwith "Non valid number of parameters"

      BoundsOnCdf =
        fun args x ->
            match args with
            | [ mean; sigma; _; _ ] ->

                let ll =
                    if sigma.lo <= 0.0 then
                        Double.PositiveInfinity
                    else
                        Continuous.Normal.CDF mean.lo sigma.lo x

                let lh =
                    if Double.IsInfinity sigma.hi then
                        0.0
                    else
                        Continuous.Normal.CDF mean.lo sigma.hi x

                let hl =
                    if sigma.lo <= 0.0 then
                        Double.PositiveInfinity
                    else
                        Continuous.Normal.CDF mean.hi sigma.lo x

                let hh =
                    if Double.IsInfinity sigma.hi then
                        0.0
                    else
                        Continuous.Normal.CDF mean.hi sigma.hi x

                let lo = List.reduce min [ ll; lh; hl; hh ]
                let hi = List.reduce max [ ll; lh; hl; hh ]
                preciseInterval lo hi
            | _ -> failwith "Invalid number of parameters"

      DetermineSplitBounds =
          fun paraBounds epsilonVar _ ->
              match paraBounds with
              | [ _; _; l; r ] ->
                  let lower, upper = l.lo, r.hi

                  let numberOfSplits =
                      Math.Max(
                          (upper - lower) / epsilonVar
                          |> Math.Ceiling
                          |> int,
                          1
                      )

                  let splittingPointsForPurePart =
                      [ for i in 0 .. numberOfSplits do
                            min (lower + double i * epsilonVar) upper ]

                  seq {
                      for j in 0 .. splittingPointsForPurePart.Length - 2 do
                          preciseInterval (splittingPointsForPurePart.[j]) (splittingPointsForPurePart.[j + 1])
                  }

              | _ -> failwith "Wrong number of args" }

// Normal Distribution
let Normal =
    { Name = "normal"
      Symbol = NORMAL
      Parameters = 2
      BoundsOnSamples =
        function
        | [ _; _ ] -> allReals
        | _ -> failwith "Non valid number of parameters"
      BoundsOnPdf =
        fun args area ->
            match args with
            | [ mean; sigma ] -> pdfNormalBounds mean sigma area
            | _ -> failwith "Non valid number of parameters"
      BoundsOnCdf =
        fun args x ->
            match args with
            | [ mean; sigma ] ->

                let ll =
                    if sigma.lo <= 0.0 then
                        Double.PositiveInfinity
                    else
                        Continuous.Normal.CDF mean.lo sigma.lo x

                let lh =
                    if Double.IsInfinity sigma.hi then
                        0.0
                    else
                        Continuous.Normal.CDF mean.lo sigma.hi x

                let hl =
                    if sigma.lo <= 0.0 then
                        Double.PositiveInfinity
                    else
                        Continuous.Normal.CDF mean.hi sigma.lo x

                let hh =
                    if Double.IsInfinity sigma.hi then
                        0.0
                    else
                        Continuous.Normal.CDF mean.hi sigma.hi x

                let lo = List.reduce min [ ll; lh; hl; hh ]
                let hi = List.reduce max [ ll; lh; hl; hh ]
                preciseInterval lo hi
            | _ -> failwith "Invalid number of parameters"
      DetermineSplitBounds =
        fun paraBounds epsilonVar _ ->
            match paraBounds with
            | [ mean; sigma ] ->
                // For now we just use the area of +- 2 *sigma and split this in intervals. In addition, we add the are [-inft, lower] and [upper, infty]
                let lower = mean.lo - 2.0 * sigma.hi
                let upper = mean.hi + 2.0 * sigma.hi

                if Double.IsFinite lower && Double.IsFinite upper then
                    let numberOfSplits =
                        Math.Max(
                            (upper - lower) / epsilonVar
                            |> Math.Ceiling
                            |> int,
                            1
                        )

                    let splittingPointsForPurePart =
                        [ for i in 0 .. numberOfSplits do
                              min (lower + double i * epsilonVar) upper ]

                    let intervals =
                        [ for j in 0 .. splittingPointsForPurePart.Length - 2 do
                              preciseInterval (splittingPointsForPurePart.[j]) (splittingPointsForPurePart.[j + 1]) ]

                    (preciseInterval Double.NegativeInfinity lower)
                    :: (preciseInterval upper Double.PositiveInfinity)
                       :: intervals
                    |> seq
                else
                    Seq.singleton Interval.allReals

            | _ -> failwith "Wrong number of args" }


let poissonPdf (lambda: double) (x: double) =
    if Double.IsPositiveInfinity(x) || x < 0.0 then
        0.0
    else
        Discrete.Poisson.PDF lambda (int x)

let poissonDensity (lambda: Interval) (x: Interval) =
    if nextLarger lambda.lo < nextSmaller lambda.hi then
        printfn $"{lambda}"
        failwith "Poisson distribution only supported for point parameters."

    let lam = nextLarger lambda.lo
    let pLo = poissonPdf lam x.lo
    let pHi = poissonPdf lam x.hi
    let pMode = poissonPdf lam lam

    if x.Contains(lam) then
        itv (min pLo pHi) (max (max pLo pHi) pMode)
        |> ensureNonnegative
    else
        itv (min pLo pHi) (max pLo pHi)
        |> ensureNonnegative

let poissonCdf (lambda: double) (x: double) =
    if x < 0.0 then
        0.0
    elif Double.IsPositiveInfinity(x) then
        1.0
    else
        Gamma.upperIncomplete (Math.Floor(x + 1.0)) lambda

// Poisson distribution
let Poisson =
    { Name = "poisson"
      Symbol = POISSON
      Parameters = 1
      BoundsOnSamples =
        function
        | [ _ ] -> preciseInterval 0.0 System.Double.PositiveInfinity
        | _ -> failwith "Invalid number of parameters"
      BoundsOnPdf =
        fun args area ->
            match args with
            | [ lambda ] -> poissonDensity lambda area
            | _ -> failwith "Invalid number of parameters"
      BoundsOnCdf =
        fun args x ->
            match args with
            | [ lambda ] ->
                let ll = poissonCdf lambda.lo x
                let lh = poissonCdf lambda.hi x
                preciseInterval (min ll lh) (max ll lh)
            | _ -> failwith "Invalid number of parameters"
      DetermineSplitBounds =
        fun paraBounds epsilonVar _ ->
            let lambda =
                match paraBounds with
                | [ lambda ] -> lambda
                | _ -> failwith "invalid number of parameters"

            let dx =
                if epsilonVar < 1.0 then
                    nextSmaller 1.0
                else
                    epsilonVar

            if Double.IsInfinity(lambda.hi) then
                seq { preciseInterval 0.0 Double.PositiveInfinity }
            else
                let upper = 2.0 * lambda.hi

                let numberOfSplits =
                    Math.Max(upper / dx |> Math.Ceiling |> int, 1)

                let splittingPointsForPurePart =
                    [ for i in 0 .. numberOfSplits do
                          min (double i * dx) upper ]

                seq {
                    for j in 0 .. splittingPointsForPurePart.Length - 2 do
                        yield preciseInterval (splittingPointsForPurePart.[j]) (splittingPointsForPurePart.[j + 1])

                    yield preciseInterval upper Double.PositiveInfinity
                } } // nothing better to return

// Beta Distribution
let Beta =
    let betaPdf (alpha: double) (beta: double) (x: double) = Continuous.Beta.PDF alpha beta x

    let betaDensity (alpha: Interval) (beta: Interval) (x: Interval) =
        if alpha.lo <> alpha.hi || beta.lo <> beta.hi then
            printfn $"{alpha}, {beta}"
            failwith "Beta distribution only supported for point parameters."

        let a = alpha.lo
        let b = beta.lo

        let res =
            hull (betaPdf a b x.lo |> point) (betaPdf a b x.hi |> point)

        let res =
            if x.Contains(0.0) then
                hull res (betaPdf a b 0.0 |> point)
            else
                res

        let res =
            if x.Contains(1.0) then
                hull res (betaPdf a b 1.0 |> point)
            else
                res

        let mode = (a - 1.0) / (a + b - 2.0)

        let res =
            if x.Contains(mode) then
                hull res (betaPdf a b mode |> point)
            else
                res

        res |> ensureNonnegative

    { Name = "beta"
      Symbol = BETA
      Parameters = 2
      BoundsOnSamples =
        function
        | [ _; _ ] -> unitInterval
        | _ -> failwith "Invalid number of parameters"
      BoundsOnPdf =
        fun args area ->
            match args with
            | [ alpha; beta ] -> betaDensity alpha beta area
            | _ -> failwith "Invalid number of parameters"
      BoundsOnCdf =
        fun args x ->
            match args with
            | [ alpha; beta ] ->
                let ll = Continuous.Normal.CDF alpha.lo beta.lo x
                let lh = Continuous.Normal.CDF alpha.lo beta.hi x
                let hl = Continuous.Normal.CDF alpha.hi beta.lo x
                let hh = Continuous.Normal.CDF alpha.hi beta.hi x
                let lo = List.reduce min [ ll; lh; hl; hh ]
                let hi = List.reduce max [ ll; lh; hl; hh ]
                preciseInterval lo hi
            | _ -> failwith "Invalid number of parameters"
      DetermineSplitBounds =
        fun _ epsilonVar _ ->
            // We can ignore the params
            let lower, upper = 0.0, 1.0

            let numberOfSplits =
                Math.Max(
                    (upper - lower) / epsilonVar
                    |> Math.Ceiling
                    |> int,
                    1
                )

            let splittingPointsForPurePart =
                [ for i in 0 .. numberOfSplits do
                      min (lower + double i * epsilonVar) upper ]

            seq {
                for j in 0 .. splittingPointsForPurePart.Length - 2 do
                    preciseInterval (splittingPointsForPurePart.[j]) (splittingPointsForPurePart.[j + 1])
            } }

// The list of all distributions supported by GuBPI
let distributions: list<PrimitiveDistribution> =
    [ Uniform
      TruncatedNormal
      Normal
      Poisson
      Beta ]

let private nameMap =
    [ for d in distributions do
          (d.Name, d) ]
    |> Map.ofSeq

let private symbolMap =
    [ for d in distributions do
          (d.Symbol, d) ]
    |> Map.ofSeq

/// For a given distribution symbol, look up the Distribution
let lookupDistributionSymbol (s: DistributionSymbol) = Map.find s symbolMap

/// For a given string, look up the Distribution. Used by the Parser
let lookupDistributionName (name: String) = Map.tryFind name nameMap
