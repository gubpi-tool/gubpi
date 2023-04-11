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
open System.IO

open Flips
open Flips.Types

open Interval
open Util
open Util.SubprocessUtil
open LinearFunction


type Opt =
    | MIN
    | MAX


type LinearProgramSolution = 
    | FiniteValue of double 
    | Infeasible 
    | Unbounded
 

/// Solves a LP by making using Flips. This fails on non-x86 machines
module FlipsSolver = 
    /// Converts a linear Function to a LinearExpression used by Flips
    let private toLinearExpression (lf: LinearFunction) (m: Map<int, Decision>) : LinearExpression =
        let { LinearFunction.Coefficients = coef; Offset = abs } = lf

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
        : LinearProgramSolution =

        // Convert to Linear Function

        
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
        for { Function = lf; Com = com; Threshold = thres } in conditions do
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
            { SolverType = SolverType.GLOP;
            MaxDuration = 10_000L;
            WriteLPFile = None;
            WriteMPSFile = None }

        let result: SolveResult = Solver.solve settings model


        // Check te result of the solver
        match result with
        | SolveResult.Optimal sol ->
            Objective.evaluate sol objective |> FiniteValue
        | SolveResult.Unknown _ ->
            // This should never happen
            failwith "No optimal solutaion found, unknown"
        | SolveResult.Unbounded _ -> Unbounded
        | SolveResult.Infeasible _ -> Infeasible

/// Solves a LP by making a call to lpsolve
module LpSolveSolver =
    
    
    let lpSolveStopwatch = System.Diagnostics.Stopwatch()

    // Computes the volume by calling the external tool vinci and parsing its output
    let private lpSolveFromString (filePath: string) : LinearProgramSolution =
        // We get the path of the GuBPI executable. By convention, the vinci execuatble is located in the same dirctory
        let lpSolvePath = 
            System.IO.Path.Join [|System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location); "lp_solve"|]

        lpSolveStopwatch.Start()
        let out = Util.SubprocessUtil.runCommandWithTimeout lpSolvePath ("-lp " + filePath + " -S1") None
        lpSolveStopwatch.Stop()

        match out with 
        | SubprocessOutcome out -> 
            if out.Contains "unbounded" then
                Unbounded
            elif out.Contains "infeasible" || out.Contains "bound contradicts earlier bounds" then
                Infeasible

            elif out.Contains "Value of objective function:" then 
                let d = (out.Split ':').[1]

                try 
                    Double.Parse(d) |> FiniteValue
                with 
                | err -> 
                    printfn "An error occured while performing analysis via lp_solve."
                    printfn $"The error was:\n%A{err}"
                    exit 0
            else
                printfn "Unexpected output by lp_solve:\n"
                printfn $"%A{out}"
                exit 0

        | SubprocessTimeout ->
            printfn "lp_solve timed out"
            exit 0
                
        | SubprocessError err -> 
            // When bounds contradict themself lp_solve sometimes gives an error instead of saying the model is infeasible. We catch this and instead return infeasible
            if err.Contains "bound contradicts earlier bounds" then
                Infeasible
            else
                printfn "An error occured while performing analysis via lp_solve."
                printfn $"The error was:\n%A{err}"
                exit 0
    
    
    let private printLinearFunction (lf: LinearFunction) : String =
        lf.Coefficients
        |> Map.toList
        |> List.map (fun (var, c) -> if c = 1.0 then "x" + string(var) else (string(c) + " " + "x" + string(var)) )
        |> List.append [string(lf.Offset)]
        |> Util.combineStringsWithSeperator " + "

    let private printLinearInequality (le : LinearInequality) : String = 
        let functionString = printLinearFunction le.Function
        let thresholdString = string(le.Threshold)

        let compString = 
            match le.Com with 
            | LEQ | LT -> "<="
            | GEQ | GT -> ">="

        functionString + " " + compString + " " + thresholdString
        
    /// Performs linear Optimization on the value of target under the conditions given by conditions and split
    /// It can either maximize or minimize the value of target.
    let optimize
        (conditions: list<LinearInequality>)
        (split: VarBoundMap)
        (target: LinearFunction)
        (o: Opt)
        : LinearProgramSolution =

        
        // Compute all variables used in this problem
        let usedVars =
            Set.unionMany (
                (target.UsedVars)
                :: List.map (fun (lie: LinearInequality) -> lie.UsedVars) conditions
            )

        let sw = new StringWriter()

        sw.Write(match o with MAX -> "max: " | MIN -> "min: ")
        sw.Write(printLinearFunction target)
        sw.Write(";\n")

        // Print the bounds for each variable
        for i in usedVars do 
            let l, u = split.[i].ToPair
            sw.Write(string(l) + " <= " + "x" + string(i) + " <= " + string(u))
            sw.Write(";\n")

        // Print all linear constraints
        for le in conditions do 
            sw.Write(printLinearInequality le)
            sw.Write(";\n")

        // Write the query string to a file
        let queryString = sw.ToString()
        let path = "query.lp"
        File.WriteAllText(path, queryString)

        lpSolveFromString path


type LpSolver = 
    | Flips 
    | Lpsolve
    
let mutable solver = Lpsolve
    
let linearProgrammingStopwatch = System.Diagnostics.Stopwatch()
    
let optimize
    (conditions: list<LinearInequality>)
    (split: VarBoundMap)
    (target: LinearFunction)
    (o: Opt)
    (isFeasible) // Set this flag, if we can assume that the conditions are valid, which allows for easier handling in the case target uses no variables
        : option<double> =
    
    linearProgrammingStopwatch.Start()
    let res = 
        if isFeasible && Set.isEmpty target.UsedVars then
            // We know the are is feasible and target uses no vars (is a constant). So just return the value of target
            FiniteValue target.Offset
        elif conditions.IsEmpty && target.UsedVars.IsEmpty then 
            FiniteValue target.Offset
        else
            match solver with
            | Flips -> FlipsSolver.optimize conditions split target o
            | Lpsolve -> LpSolveSolver.optimize conditions split target o
            
    linearProgrammingStopwatch.Stop()
    
    match res with
    | FiniteValue x -> Some x
    | Unbounded -> match o with MAX -> Some Double.PositiveInfinity | MIN -> Some Double.NegativeInfinity
    | Infeasible -> None
           
            

/// Uses opt to compute both lower and upper bounds on target
let computeBounds (conditions: list<LinearInequality>) (split: VarBoundMap) (target: LinearFunction)  =
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

    optimize conditions split simpleLF MAX false
    |> Option.isSome