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
        | SVFun (PDF_NORMAL, [| SVCon mean; sigma; W |]) | SVFun (PDF_NORMAL, [| W; sigma; SVCon mean |]) -> 
            // We assume that either the mean or the argument is a constant. The other paramaters can be arbitrary symbolic values

            // Convert the two symbolic values to score closures
            let scoreClosureSigma = toScoreClosure sigma 
            let scoreClosureW = toScoreClosure W

            // Assumes that sigma_l >= 0.0
            let lowerBound (sigma_l, sigma_u) (w_l, w_u) =
                // The smallest value is attained at the position in the w-interval that is furthest aways from the mean
                let pointFurthestFromMean = if abs(w_l - mean) > abs(w_u - mean) then w_l else w_u

                // The function \sigma -> pdfnormal(mean, sigma, pointFurthestFromMean) is monotonically increasing on (0, t) and monotonically decreasing on (t, \infty) for some value t (which we can compute but do not need here)
                // The minimum is thus attained at one of the two endpoints of the sigma-interval (as sigma_l >= 0.0)
                min (pdfNormal (mean, sigma_l, pointFurthestFromMean)) (pdfNormal (mean, sigma_u, pointFurthestFromMean))

            // Assumes that sigma_l >= 0.0
            let upperBound (sigma_l, sigma_u) (w_l, w_u) =
                assert (sigma_l >= 0.0)

                if w_l <= mean && mean <= w_u then 
                    // The mean is included in the w-interval. The maximal value is thus attained at that mean with the smallest possible sigma (as we assume that sigma_l >= 0.0)
                    pdfNormal (mean, sigma_l, mean)
                else
                    let pointClosestToMean = if w_u < mean then w_u else w_l
                    // We can assume that pointClosestToMean <> mean 
                    // We know that the maximal value is attained at pointClosestToMean

                    // Compute the value of sigma such that pdfnormal(mean, sigma, pointClosestToMean) is maximal
                    let sigmaThatMaximisesPdf = abs (mean - pointClosestToMean)

                    // Note: The function \sigma -> pdfnormal(mean, sigma, pointClosestToMean) is monotonically increasing on (0, sigmaThatMaximisesPdf) and monote decreasing on (sigmaThatMaximisesPdf, \infty)
                    if sigma_l <= sigmaThatMaximisesPdf && sigmaThatMaximisesPdf <= sigma_u then 
                        // The value for sigma that maximises pdfnormal(mean, sigma, pointClosestToMean) is contained in the sigma-interval
                        pdfNormal (mean, sigmaThatMaximisesPdf, pointClosestToMean) 
                    else 
                        // The max value is attained at one of the two endpoint of the sigma-interval (as sigma_l >= 0.0)
                        max (pdfNormal (mean, sigma_l, pointClosestToMean) ) (pdfNormal (mean, sigma_u, pointClosestToMean) )


            let bounds (args : list<Interval>) =
                // Separete the bounds used by sigma and W
                let argsForSigma = args[0..scoreClosureSigma.LinearParts.Length - 1]
                let argsForW = args[scoreClosureSigma.LinearParts.Length..]

                // Compute the bounds on sigma and take the abs. This ensures that all values are non-negative.
                let sigmaBounds = (scoreClosureSigma.BoundNonLinearPart argsForSigma).Abs().ToPair

                let wBounds = (scoreClosureW.BoundNonLinearPart argsForW).ToPair

                let lower = lowerBound sigmaBounds wBounds
                let upper = upperBound sigmaBounds wBounds
                itv lower upper |> ensureNonnegative

            { LinearParts = scoreClosureSigma.LinearParts @ scoreClosureW.LinearParts
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
