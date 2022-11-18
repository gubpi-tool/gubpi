(****************************************************************************************)
(*                                                                                      *)
(*                                      PrimitiveFunctions.fs                           *)
(*                                                                                      *)
(****************************************************************************************)
(*                                                                                      *)
(* The list of primitive functions supported by GuBPI.                                  *)
(*                                                                                      *)
(****************************************************************************************)

module PrimitiveFunctions

open System
open Interval

type FunctionSymbol =
    | NEG
    | ADD
    | SUB
    | MUL
    | DIV
    | SQRT
    | EXP
    | LOG
    | SIG
    | ABS
    | MAX
    | MIN
    | PDF_NORMAL

/// Type of the representation of a primtive Function
type PrimitiveFunction =
    {
      // The name of the Function as it is used in terms
      Name: String

      // The Function Symbol, used to identify the primitiveFunction via an integer
      Symbol: FunctionSymbol

      // The number of arguments for the functions
      Arity: int

      // Given Values for each argument compute the value of the function
      ComputeDouble: array<double> -> double

      // Given bounds for each argument, compute bounds on the function
      ComputeInterval: array<Interval> -> Interval }

let Neg =
    { Name = "neg"
      Symbol = NEG
      Arity = 1
      ComputeDouble = fun args -> -args.[0]
      ComputeInterval = fun args -> -args.[0] }

let Add =
    { Name = "add"
      Symbol = ADD
      Arity = 2
      ComputeDouble = fun args -> args.[0] + args.[1]
      ComputeInterval = fun args -> args.[0] + args.[1] }


let Sub =
    { Name = "sub"
      Symbol = SUB
      Arity = 2
      ComputeDouble = fun args -> args.[0] - args.[1]
      ComputeInterval = fun args -> args.[0] - args.[1] }


let Mul =
    { Name = "mul"
      Symbol = MUL
      Arity = 2
      ComputeDouble = fun args -> args.[0] * args.[1]
      ComputeInterval = fun args -> args.[0] * args.[1] }

let Div =
    { Name = "div"
      Symbol = DIV
      Arity = 2
      ComputeDouble = fun args -> args.[0] / args.[1]
      ComputeInterval = fun args -> args.[0] / args.[1] }

let Sqrt =
    { Name = "sqrt"
      Symbol = SQRT
      Arity = 1
      ComputeDouble = fun args -> sqrt args.[0]
      ComputeInterval = fun args -> args.[0].Sqrt() }

let Exp =
    { Name = "exp"
      Symbol = EXP
      Arity = 1
      ComputeDouble = fun args -> Math.Exp args.[0]
      ComputeInterval = fun args -> args.[0].Exp() }

let Log =
    { Name = "log"
      Symbol = LOG
      Arity = 1
      ComputeDouble = fun args -> Math.Log args.[0]
      ComputeInterval = fun args -> args.[0].Log() }

let pdfNormal ((mean, sigma, x): double * double * double) : double =
    let sigma = abs sigma
    if Double.IsFinite mean && Double.IsFinite sigma then
        if Double.IsFinite x then
            Math.Exp((-(x - mean) * (x - mean)) / (2.0 * sigma * sigma))
            / (Math.Sqrt(2.0 * Math.PI) * sigma)
        else
            0.0
    else
        failwith "Normal distribution with infinite mean or variance"

let pdfNormalBounds (mean: Interval) (sigma: Interval) (x: Interval) =
    // Instead of computing bounds by evaluating the pdfnormal in interval arithmatic, we compute more precise bounds by leveraging the structure of the normal PDF

    // We take the abs of sigma
    let sigma = sigma.Abs()

    let lowerBound = 
        // The smallest value of pdfnormal(mean, sigma, x) is obtained at those values (mean, x) that are furthest apart from each other 
        // Find a pair (mean, x) within the interval bounds that is furthest away from each other
        let meanFurthest, xFurthest = 
            [(mean.lo, x.lo); (mean.lo, x.hi); (mean.hi, x.lo); (mean.hi, x.lo) ]
            |> List.maxBy (fun (x, y) -> abs (x - y))

        // The function \sigma -> pdfnormal(furthest, sigma, x_furthest) is monotonically increasing on (0, t) and monotonically decreasing on (t, \infty) for some value t (which we can compute but do not need here)
        // The minimum is thus attained at one of the two endpoints of the sigma-interval (as we have ensured that sigma.lo >= 0.0 by taking the abs)
        min (pdfNormal (meanFurthest, sigma.lo, xFurthest)) (pdfNormal (meanFurthest, sigma.hi, xFurthest))

    let upperBound =
        // The largest value of pdfnormal(mean, sigma, x) is obtained at those values (mean, x) that are closest together
        // Find a pair (mean, x) within the interval bounds that are as close together as possible (possibly identical)
        let meanClosest, xClosest = 
            match Interval.tryIntersect mean x with 
            | Some i -> 
                // A point shared in both intervals (distance = 0)
                i.lo, i.lo
            | None -> 
                // The closest values are either (mean.lo, x.hi) or (mean.hi, x.lo)
                [(mean.lo, x.hi); (mean.hi, x.lo)]
                |> List.minBy (fun (x, y) -> abs (x - y))

        // For the given (mean_closest, x_closest) we can easly compute the sigma such that pdfnormal(mean_closest, sigma, x_closest) is maximal using simple calculus
        let optimalSigma = abs (meanClosest - xClosest)

        // Note: The function \sigma -> pdfnormal(mean_closest, sigma, x_closest) is monotonically increasing on (0, optimalSigma) and monotonically decreasing on (optimalSigma, \infty)
        if sigma.lo <= optimalSigma && optimalSigma <= sigma.hi then 
            // optimalSigma is conatained in the interval bounds for sigma
            pdfNormal (meanClosest, optimalSigma, xClosest) 
        else 
            // The max value is attained at one of the two endpoint of the sigma-interval (as we have ensured that sigma.lo >= 0.0 by taking the abs)
            max (pdfNormal (meanClosest, sigma.lo, xClosest) ) (pdfNormal (meanClosest, sigma.hi, xClosest) )

    Interval(lowerBound, upperBound)


let PdfNormal =
    { Name = "pdfnormal"
      Symbol = PDF_NORMAL
      Arity = 3
      ComputeDouble = fun args -> pdfNormal (args.[0], args.[1], args.[2])
      ComputeInterval = fun args -> pdfNormalBounds (args.[0]) (args.[1]) (args.[2]) }

/// All primitive functions in use. To add support for a new function add it to tis list
let primFunctions =
    [ Neg
      Add
      Sub
      Mul
      Div
      Sqrt
      Exp
      Log
      PdfNormal ]


let private nameMap =
    [ for d in primFunctions do
          (d.Name, d) ]
    |> Map.ofSeq


/// For a given function symbol, look up the function
let lookupFunctionSymbol (s: FunctionSymbol) =
    List.find (fun x -> x.Symbol = s) primFunctions


/// For a given string, look up the function with that symbol. Used by the Parser
let lookupFunctionName (name: String) = Map.tryFind name nameMap
