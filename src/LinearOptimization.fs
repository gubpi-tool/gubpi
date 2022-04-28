(****************************************************************************************)
(*                                                                                      *)
(*                                      LinearOptimization.fs                           *)
(*                                                                                      *)
(****************************************************************************************)
(*                                                                                      *)
(* A F# interface to use Flips (used for linear optimization)                           *)
(*                                                                                      *)
(****************************************************************************************)
module LinearOptimization

open System

open Flips
open Flips.Types

open Interval
open Util
open LinearFunction


type Opt =
    | MIN
    | MAX

let linearOptStopwatch = System.Diagnostics.Stopwatch()

/// Converts a linear Function to a LinearExpression used by Flips
let private toLinearExpression (lf: LinearFunction) (m: Map<int, Decision>) : LinearExpression =
    let { LinearFunction.Coefficients = coef
          Offset = abs } =
        lf

    let mutable res = LinearExpression.Empty

    for KeyValue (var, c) in coef do
        // Find the decision variable
        let d = Map.find var m
        res <- AddDecision((c, d), res)
    // Add additive Term
    res <- AddFloat(abs, res)
    res




/// Performs linear Optimization on the value of target under the conditions given by conditions and split
/// It can either maximize or minimize the value of target.
let optimize
    (conditions: list<LinearInequality>)
    (split: VarBoundMap)
    (target: LinearFunction)
    (o: Opt)
    (isFeasible)
    : option<double> =

    linearOptStopwatch.Start()
    // Convert to Linear Function

    if isFeasible && Set.isEmpty target.UsedVars then
        // We know the are is feasible and target uses no vars (is a constant). So just return the value of target
        Some target.Offset
    else
        // Compute all variables used in this problem
        let usedVars =
            Set.unionMany (
                (target.UsedVars)
                :: List.map (fun (lie: LinearInequality) -> lie.UsedVars) conditions
            )

        // Create a decision for each variable (a decision is the internal representation of a variable bu Flips).
        let decMap =
            [ for i in usedVars do
                  let l, r = split.[i].ToPair

                  i, Decision.createContinuous ("x_" + string (i)) l r ]
            |> Map.ofList

        // Create the objective function, this is just target converted to Flips and either the MAX or Min objective

        let le = toLinearExpression target decMap

        let objective =
            Objective.create
                "objective"
                (match o with
                 | MIN -> ObjectiveSense.Minimize
                 | MAX -> ObjectiveSense.Maximize)
                le

        // Create the model
        let mutable model = Model.create objective


        // Add all constraints to the model by converting to Flips
        for { Function = lf
              Com = com
              Threshold = thres } in conditions do
            let left = toLinearExpression lf decMap
            let right = AddFloat(thres, Empty)

            let ce =
                match com with
                | LEQ -> Inequality(left, Inequality.LessOrEqual, right)
                | LT -> Inequality(left, Inequality.LessOrEqual, right)
                | GT -> Inequality(left, Inequality.GreaterOrEqual, right)
                | GEQ -> Inequality(left, Inequality.GreaterOrEqual, right)

            let c = Constraint.create "constraint" ce
            model <- Model.addConstraint c model

        // We solve using GLOP
        let settings =
            { SolverType = SolverType.GLOP
              MaxDuration = 10_000L
              WriteLPFile = None
              WriteMPSFile = None }

        let result: SolveResult = Solver.solve settings model

        linearOptStopwatch.Stop()

        // Check te result of the solver
        match result with
        | Optimal sol -> Objective.evaluate sol objective |> Some
        | Unknown _ ->
            // This should never happen
            failwith "No optimal solutaion found, unknown"
        | Unbounded _ ->
            Some(
                if o = MIN then
                    Double.NegativeInfinity
                else
                    Double.PositiveInfinity
            )
        | Infeasible _ -> None




/// Uses opt to compute both lower and upper bounds on target
let computeBounds (conditions: list<LinearInequality>) (split: VarBoundMap) (target: LinearFunction) =
    let min =
        optimize conditions split target MIN true
        |> Option.get

    let max =
        optimize conditions split target MAX true
        |> Option.get

    Interval(min, max)

/// Check if the area given by conditions and split is non-empty.
let isFeasible (conditions: list<LinearInequality>) (split: VarBoundMap) =
    // We can check if the area is feasible by computing the max (or min) of the 0 function
    let simpleLF = LinearFunction.FromConstant(0.0)

    match optimize conditions split simpleLF MAX false with
    | Some _ -> true
    | None -> false
