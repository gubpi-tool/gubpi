(****************************************************************************************)
(*                                                                                      *)
(*                                      ScoreClosure.fs                                 *)
(*                                                                                      *)
(****************************************************************************************)
(*                                                                                      *)
(* Splits a Score Value into a combination of linear functions                          *)
(*                                                                                      *)
(****************************************************************************************)
module ScoreClosure

open Interval
open LinearFunction
open PrimitiveFunctions
open Expression

/// A Score closure consist of a list of linear function that represent the linear subparts of a composed function
/// Given bounds on each of the linear function, BoundNonLinearPart returns a bound on the composed function
/// In particular, the argument to BoundNonLinearPart is a function with the same length as LinearParts
type ScoreClosure =
    { LinearParts: list<LinearFunction>
      // Given bounds on the linear parts, compute bounds on the overall result of the function application.
      BoundNonLinearPart: list<Interval> -> Interval }

/// Take part [i, n] from a list
let private sublist sequ (i: int) (n: int) =
    sequ |> List.skip i |> List.take (n - i + 1)

/// Converts a symbolic Value to a score closure by identify linear parts that are as large as possible
let rec toScoreClosure (V: SymbolicValue) =

    match V.TryToIntervalLinearFunction() with
    | Some l ->
        // The symbolic value can already be translated to a IntervalLinearFunction
        if Set.isEmpty l.Linear.UsedVars then
            // The function is a constant
            let bounds _ = precisely l.Linear.Offset + l.AddedInterval

            { LinearParts = List.empty
              BoundNonLinearPart = bounds }
        else
            // If the function is non constant, we remove the interval (to obtain the linear part)
            // BoundNonLinearPart then add the interval to the bounds on the linear function

            let bounds x =
                match x with
                | [ iv ] -> iv + l.AddedInterval
                | _ -> failwith "Not possible"

            { LinearParts = [ l.Linear ]
              BoundNonLinearPart = bounds }

    | None ->
        // The function could not be translated to a linear function directly. Match on the top operation
        match V with
        | SVFun (PDF_NORMAL, [| mean; sigma; W |]) -> 
            // Convert the symbolic values to score closures
            let scoreClosureMean = toScoreClosure mean 
            let scoreClosureSigma = toScoreClosure sigma 
            let scoreClosureW = toScoreClosure W

            let bounds (args : list<Interval>) =
                // Separate the arguments used to compute bounds on mean, sigma, and W
                let argsForMean = args[0..scoreClosureMean.LinearParts.Length - 1]
                let argsForSigma = args[scoreClosureMean.LinearParts.Length..scoreClosureMean.LinearParts.Length + scoreClosureSigma.LinearParts.Length - 1]
                let argsForW = args[scoreClosureMean.LinearParts.Length + scoreClosureSigma.LinearParts.Length..]

                // Compute bounds on all arguments
                let meanBounds = scoreClosureMean.BoundNonLinearPart argsForMean
                let sigmaBounds = scoreClosureSigma.BoundNonLinearPart argsForSigma
                let wBounds = scoreClosureW.BoundNonLinearPart argsForW

                pdfNormalBounds meanBounds sigmaBounds wBounds

            { LinearParts = scoreClosureMean.LinearParts @ scoreClosureSigma.LinearParts @ scoreClosureW.LinearParts
              BoundNonLinearPart = bounds }

        | SVFun (ADD, [| W1; W2 |]) ->
            // For addition, we convert both parts to a scoreClosure, take the union of the linear parts of both and BoundNonLinearPart just adds the bounds for each size
            let recRes1 = toScoreClosure W1
            let recRes2 = toScoreClosure W2

            let linearFunctions =
                recRes1.LinearParts @ recRes2.LinearParts

            let bounds args =
                let first =
                    sublist args 0 (recRes1.LinearParts.Length - 1)

                let second =
                    sublist args recRes1.LinearParts.Length (args.Length - 1)

                recRes1.BoundNonLinearPart first + recRes2.BoundNonLinearPart second

            { LinearParts = linearFunctions
              BoundNonLinearPart = bounds }
        | SVFun (SUB, [| W1; W2 |]) ->
            // Similar to the case of addition, but using subtraction instead of addition
            let recRes1 = toScoreClosure W1
            let recRes2 = toScoreClosure W2

            let linearFunctions =
                recRes1.LinearParts @ recRes2.LinearParts

            let bounds args =
                let first =
                    sublist args 0 (recRes1.LinearParts.Length - 1)

                let second =
                    sublist args recRes1.LinearParts.Length (args.Length - 1)

                recRes1.BoundNonLinearPart first - recRes2.BoundNonLinearPart second

            { LinearParts = linearFunctions
              BoundNonLinearPart = bounds }
        | SVFun (MUL, [| W1; W2 |]) ->
            // Similar to the case of addition, but using multiplication instead of addition
            let recRes1 = toScoreClosure W1
            let recRes2 = toScoreClosure W2

            let linearFunctions =
                recRes1.LinearParts @ recRes2.LinearParts

            let bounds args =
                let first =
                    sublist args 0 (recRes1.LinearParts.Length - 1)

                let second =
                    sublist args recRes1.LinearParts.Length (args.Length - 1)

                recRes1.BoundNonLinearPart first * recRes2.BoundNonLinearPart second

            { LinearParts = linearFunctions
              BoundNonLinearPart = bounds }
        | SVFun (DIV, [| W1; W2 |]) ->
            // Similar to the case of addition, but using division instead of addition
            let recRes1 = toScoreClosure W1
            let recRes2 = toScoreClosure W2

            let linearFunctions =
                recRes1.LinearParts @ recRes2.LinearParts

            let bounds args =
                let first =
                    sublist args 0 (recRes1.LinearParts.Length - 1)

                let second =
                    sublist args recRes1.LinearParts.Length (args.Length - 1)

                recRes1.BoundNonLinearPart first / recRes2.BoundNonLinearPart second

            { LinearParts = linearFunctions
              BoundNonLinearPart = bounds }
        | _ -> failwith "Not supported"
