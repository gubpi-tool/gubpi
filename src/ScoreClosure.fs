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
        | SVFun (PDF_NORMAL, [| SVCon mean; SVCon sigma; W |]) | SVFun (PDF_NORMAL, [| W; SVCon sigma; SVCon mean |]) ->
            // For the normal function, we assume that either the mean or the argument is constant.
            // We then convert the argument to a score closure.
            // Given bounds on the argument, BoundNonLinearPart, the computes lower and upper approximations on the value of the normal pdf on that interval

            let recRes = toScoreClosure W

            // The lower bound on the pdf normal is taken at one of the ends of the interval
            let normLowerApprox =
                fun x y -> min (pdfNormal (mean, sigma, x)) (pdfNormal (mean, sigma, y))

            // The upper bound on the pdf normal is taken at either one of the ends of the interval, or at the mean (if included in the interval)
            let normUpperApprox x y =
                if x <= mean && mean <= y then
                    pdfNormal (mean, sigma, mean)
                else
                    max (pdfNormal (mean, sigma, x)) (pdfNormal (mean, sigma, y))


            let bounds args =
                let (l, u) = (recRes.BoundNonLinearPart args).ToPair
                let lower = normLowerApprox l u
                let upper = normUpperApprox l u
                itv lower upper |> ensureNonnegative

            { LinearParts = recRes.LinearParts
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
