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
    if Double.IsFinite mean && Double.IsFinite sigma then
        if Double.IsFinite x then
            Math.Exp((-(x - mean) * (x - mean)) / (2.0 * sigma * sigma))
            / (Math.Sqrt(2.0 * Math.PI) * sigma)
        else
            0.0
    else
        failwith "Normal distribution with infinite mean or variance"

let pdfNormalBounds (mean: Interval) (sigma: Interval) (x: Interval) =
    (point (sqrt (2.0 * Math.PI)) * sigma).Inverse()
    * (- (x - mean).Squared()
       / (precisely 2.0 * sigma.Squared()))
        .Exp()

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
