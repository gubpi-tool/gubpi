(****************************************************************************************)
(*                                                                                      *)
(*                                      Evaluation.fs                                   *)
(*                                                                                      *)
(****************************************************************************************)
(*                                                                                      *)
(* Performs Symbolic Evaluation of a Term and Applies Static Analysis to Fixpoints      *)
(*                                                                                      *)
(****************************************************************************************)
module Evaluation

open System.Collections.Generic

open Interval
open Util
open UnionFind
open Expression
open ConstraintSolver


type PathFragment =
    { Guards: list<Guard>
      ScoreValues: list<SymbolicValue>
      SampleDistributions: VarDistMap }

    static member empty =
        { Guards = []
          ScoreValues = []
          SampleDistributions = Map.empty }

    member this.UsedVars =
        let usedScore =
            this.ScoreValues
            |> List.map (fun (x: SymbolicValue) -> x.UsedVars)
            |> Set.unionMany

        let usedBranch =
            this.Guards
            |> List.map (fun (x: Guard) -> x.UsedVars)
            |> Set.unionMany

        let usedInDistMap =
            this.SampleDistributions
            |> Map.toSeq
            |> Seq.map fst
            |> set

        Set.unionMany [ usedScore
                        usedBranch
                        usedInDistMap ]

/// A ProgramPath is a PathFragment but, additionally, has a value
type ProgramPath =
    { Value: SymbolicValue
      Fragment: PathFragment }

    member this.Guards = this.Fragment.Guards
    member this.ScoreValues = this.Fragment.ScoreValues
    member this.SampleDistributions = this.Fragment.SampleDistributions

    member this.UsedVars =
        let usedV = this.Value.UsedVars
        let usedFragment = this.Fragment.UsedVars

        Set.unionMany [ usedV; usedFragment ]

    /// Normalizes the path. This ensures that the sample variables used in the path are in [0, ...,n-1] where n is the number of sample variables
    member this.Normalize() =
        let usedVars = this.UsedVars
        let mutable m = Map.empty
        let mutable count = 0
        let mutable newDistMap = Map.empty

        for i in usedVars do
            m <- Map.add i count m
            newDistMap <- Map.add count (this.SampleDistributions.[i]) newDistMap
            count <- count + 1

        // Replace the vars in each Guard
        let c =
            List.map (fun (g: Guard) -> { g with Value = g.Value.ReplaceVars m }) this.Guards

        // Replace the vars in each Score Value
        let s =
            List.map (fun (x: SymbolicValue) -> x.ReplaceVars m) this.ScoreValues

        let res = this.Value.ReplaceVars m

        { Value = res
          Fragment =
            { Guards = c
              ScoreValues = s
              SampleDistributions = newDistMap } },
        count - 1

/// Checks if the Guard is definitely sat and can thus be removed
let private isGuardAlwaysSat bounds (g: Guard) =
    let v =
        (g.Value.EvalIntervalMap bounds).ExtractInterval

    match g.Com with
    | LEQ -> v.hi <= g.Threshold
    | LT -> v.hi < g.Threshold
    | GEQ -> v.lo >= g.Threshold
    | GT -> v.lo > g.Threshold

/// Checks if the Guard is definitely unsat, i.e., the entire program path containing this path is is infeasible
let isGuardAlwaysUnsat bounds (g: Guard) =
    let v =
        (g.Value.EvalIntervalMap bounds).ExtractInterval

    match g.Com with
    | LEQ -> v.lo > g.Threshold
    | LT -> v.lo >= g.Threshold
    | GEQ -> v.hi < g.Threshold
    | GT -> v.hi <= g.Threshold

/// Cleans up a path by removing guards that are always satisfied and restricts to those variables that are used
/// If some path is always unsatisfiable, it returns NONE
let cleanUpPath (p: ProgramPath) =

    // The bounds for all variables occuring in this path
    let bounds =
        p.SampleDistributions
        |> Map.map (fun _ x -> x.Bounds)

    if List.exists (isGuardAlwaysUnsat bounds) p.Guards then
        // This guard is unsat, so the entire path is infeasible, return NONE
        None
    else
        // Filter out the guards that are always sat as we do not need to consider them
        let unsureGuards =
            List.filter (isGuardAlwaysSat bounds >> not) p.Guards

        // Compute the variables that are actually used and restrict the SampleDistributions to those variables
        let actuallyUsedVars =
            Set.unionMany [ p.Value.UsedVars
                            (unsureGuards
                             |> List.map (fun x -> x.UsedVars)
                             |> Set.unionMany)
                            (p.ScoreValues
                             |> List.map (fun x -> x.UsedVars)
                             |> Set.unionMany)
                            (p.SampleDistributions
                             |> Map.toSeq
                             |> Seq.map (fun (_, d) -> d.UsedVars)
                             |> Set.unionMany) ]

        { p with
            Fragment =
                { Guards = unsureGuards
                  ScoreValues = p.ScoreValues
                  SampleDistributions = Map.filter (fun k _ -> Set.contains k actuallyUsedVars) p.SampleDistributions } }
        |> Some

/// Given a program Path, this function tries to identify fragments of that path that are independent of the return value and only impact the weight.
/// Such fragments can be analysed separately.
let extractIndependentPathFragments (p: ProgramPath) : ProgramPath * list<PathFragment> =
    // . There can be guards that contain no var
    let (p, maxVar) = p.Normalize()

    // We use a union find structure to find sample variables that are used in the same guard or score value or the return value of the path
    let uf = UnionFind(maxVar + 1)
    let usedVarVal = p.Value.UsedVars
    let mutable usedVarsGuards =
        List.map (fun (g: Guard) -> g.UsedVars) p.Guards
    let mutable usedVarsScores =
        List.map (fun (x: SymbolicValue) -> x.UsedVars) p.ScoreValues
        
    uf.UnionMany usedVarVal
    List.iter uf.UnionMany usedVarsScores
    List.iter uf.UnionMany usedVarsGuards

    for (v, d) in p.SampleDistributions |> Map.toSeq do
        uf.UnionMany(Set.add v d.UsedVars)

    // Extract the union-find partition.
    // Sample variables in separate partitions can be split in separate fragments.
    let partition = uf.ExtractPartition()

    let mutable main =
        { Value = p.Value
          Fragment = PathFragment.empty }

    let mutable fragments = []

    for part in partition do
        // Compute all guards using variables in part
        let guards =
            List.filter
                (fun (g: Guard) ->
                    Set.intersect (g.UsedVars) part
                    |> Set.isEmpty
                    |> not)
                p.Guards

        // Compute all score values using variables in part
        let scoreValues =
            List.filter
                (fun (x: SymbolicValue) ->
                    Set.intersect x.UsedVars part
                    |> Set.isEmpty
                    |> not)
                p.ScoreValues

        // Check if this is the main path, i.e., uses variables that are used in the value of the path 
        if Set.intersect usedVarVal part
           |> Set.isEmpty
           |> not then
            // If yes, construct the main path
            main <-
                { main with
                    Fragment =
                        { Guards = guards
                          ScoreValues = scoreValues
                          SampleDistributions = Map.filter (fun k _ -> Set.contains k part) p.SampleDistributions } }
        else
            // Otherwise construct a path fragment and add it to the list
            let fragment =
                { Guards = guards
                  ScoreValues = scoreValues
                  SampleDistributions = Map.filter (fun k _ -> Set.contains k part) p.SampleDistributions }

            fragments <- fragment :: fragments

    // There can be guards and score Values that use no variables. We add them as a special path fragment
    // Add a special fragment for all guards that contain no variable
    let varFreeGuards =
        List.filter (fun (g: Guard) -> Set.isEmpty g.UsedVars) p.Guards

    let varFreeScores =
        List.filter (fun (v: SymbolicValue) -> Set.isEmpty v.UsedVars) p.ScoreValues

    if (List.isEmpty varFreeGuards |> not)
       || (List.isEmpty varFreeScores |> not) then
        let fragment =
            { Guards = varFreeGuards
              ScoreValues = varFreeScores
              SampleDistributions = Map.empty }

        fragments <- fragment :: fragments

    main, fragments


/// Contains the necessary evaluation information used for the symbolic evaluation of a term
/// This includes the current path fragment and a fresh ID for the next sample variable
type Config =
    { NextSampleVar: int
      Fragment: PathFragment }
    member this.AddGuard(guard: Guard) =
        { this with Fragment = { this.Fragment with Guards = guard :: this.Fragment.Guards } }

    member this.AddScore(factor: SymbolicValue) =
        { this with Fragment = { this.Fragment with ScoreValues = factor :: this.Fragment.ScoreValues } }

    member this.AddSampleVar(var: int, dist: SymbolicDistribution) =
        { this with
            Fragment = { this.Fragment with SampleDistributions = Map.add var dist this.Fragment.SampleDistributions } }

    member this.FreshSampleVar() =
        this.NextSampleVar, { this with NextSampleVar = this.NextSampleVar + 1 }

/// Performs One Step of the SPCF symbolic evaluation. Returns new configuration and resulting term.
let rec oneStepReduction (config: Config) (expr: Expr) : list<Expr * Config> =
    // We match on the structure of the term. The behavior in each case follows the symbolic SPCF evaluation. 
    match expr with
    | Application (M, N) ->
        if M.IsValue then
            if N.IsValue then
                match M with
                | Abstraction (x, _, M') -> [ (M'.Subst x N, config) ]
                | Fixpoint (f, x, t, M') -> [ ((M'.Subst f (Fixpoint(f, x, t, M'))).Subst x N, config) ]
                | _ -> failwith "Should not be possible for well typed terms"
            else
                List.map (fun (t, c) -> Application(M, t), c) (oneStepReduction config N)
        else
            List.map (fun (t, c) -> Application(t, N), c) (oneStepReduction config M)
    | Conditional (M, N, P) ->
        if M.IsValue then
            let scrutinee = M.ToSymbolicValue()

            let thenGuard =
                { Value = scrutinee
                  Com = Compare.LEQ
                  Threshold = 0.0 }

            let elseGuard =
                { Value = scrutinee
                  Com = Compare.GT
                  Threshold = 0.0 }

            // Only add a guard if it passes a preliminary sat check
            let bounds =
                config.Fragment.SampleDistributions
                |> Map.map (fun _ x -> x.Bounds)

            if isGuardAlwaysUnsat bounds thenGuard then
                []
            else
                [ (N, config.AddGuard thenGuard) ]
            @ if isGuardAlwaysUnsat bounds elseGuard then
                  []
              else
                  [ (P, config.AddGuard elseGuard) ]
        else
            List.map (fun (t, c) -> Conditional(t, N, P), c) (oneStepReduction config M)
    | Score (M) ->
        if M.IsValue then
            let symM = M.ToSymbolicValue()
            [ (M, config.AddScore symM) ]
        else
            List.map (fun (t, c) -> Score(t), c) (oneStepReduction config M)
    | Sample (dist, args) ->
        if List.forall (fun (x: Expr) -> x.IsValue) args then
            let sv, newC = config.FreshSampleVar()

            let symDist =
                { SymbolicDistribution.Type = dist
                  Parameters = List.map (fun (x: Expr) -> x.ToSymbolicValue()) args }

            [ (SampleVariable sv, newC.AddSampleVar(sv, symDist)) ]
        else
            List.map (fun (t, c) -> Sample(dist, t), c) (oneStepList config args)
    | ConsList (M, N) ->
        if M.IsValue then
            // N cannot be a value
            List.map (fun (t, c) -> ConsList(M, t), c) (oneStepReduction config N)
        else
            List.map (fun (t, c) -> ConsList(t, N), c) (oneStepReduction config M)
    | FoldList (M, N, x, xs, P) ->
        if M.IsValue then
            match M with
            | EmptyList _ -> [ (N, config) ]
            | ConsList (first, rest) ->
                // Both first and rest are values
                [ ((P.Subst x first).Subst xs rest, config) ]
            | _ -> failwith "Scrutinee must be a list"
        else
            List.map (fun (t, c) -> FoldList(t, N, x, xs, P), c) (oneStepReduction config M)
    | PrimF (name, args) ->
        if List.forall (fun (x: Expr) -> x.IsValue) args then
            // Can reduce further
            [ (SymPrimF(name, args), config) ]
        else
            List.map (fun (t, c) -> PrimF(name, t), c) (oneStepList config args)
    | Tuple args -> List.map (fun (t, c) -> Tuple t, c) (oneStepList config args)
    | TupleMatch (vars, N, P) ->
        if N.IsValue then
            match N with
            | Tuple args ->
                let subst = List.zip vars args |> Map.ofList
                [ P.ParaSubst subst, config ]
            | _ -> failwith "Impossible for well typed terms"
        else
            List.map (fun (t, c) -> TupleMatch(vars, t, P), c) (oneStepReduction config N)
    | _ -> failwith $"Cannot reduce a value {expr}"

and oneStepList (config: Config) (exprs: Expr list) : list<list<Expr> * Config> =
    let index =
        List.findIndex (fun (x: Expr) -> x.IsValue |> not) exprs

    let temp = exprs.[index] |> oneStepReduction config
    List.map (fun (t, c) -> List.mapi (fun i e -> if i = index then t else e) exprs, c) temp



/// Performs the symbolic Evaluation of a term.
let evalQueue (targetDepth: int) (M: Expr) =
    
    // The method uses a queue opposed to being recursive so intermediate output on the number of terms in the queue is possible.
    
    
    let mutable paths = List.empty

    // The queue maintains the current expression, the configuration (containing the symbolic guards and symbolic values already used) and the depth of the exploration
    // The depth is either Some(d) at which case exploration is stopped if d > targetDepth or None (in which case exploration is only stopped once a value is reached)
    // This is used to remove fixpoint and set the depth to None (as terms without fixpoints are strongly normalizing). 
    let queue = new Queue<Expr * Config * option<int>>()
    
    // For the initial configuration the path is empty and the next sample-variable is 0 
    let initConfig =
        { Config.Fragment = PathFragment.empty
          NextSampleVar = 0 }

    queue.Enqueue(M, initConfig, Some 0)

    let mutable currentMaxDepth = -1 // Used to construct Outputs

    printfn "Start eval..."

    while queue.Count <> 0 do
        let (expr, config, currentDepth) = queue.Dequeue()

        if currentDepth.IsSome
           && currentDepth.Value > currentMaxDepth then
            // Update the current depth for printout
            currentMaxDepth <- currentDepth.Value

        if GlobalConstants.outputEvalDepth then
            System.Console.SetCursorPosition(0, System.Console.CursorTop)
            printf "Currently on depth %i. In the queue %i" currentMaxDepth queue.Count

        if expr.IsValue then
            // The expression is a value, so we found a terminating symbolic path
            let path =
                { ProgramPath.Value = expr.ToSymbolicValue()
                  Fragment = config.Fragment }

            paths <- path :: paths

        elif Option.isSome currentDepth
             && (Option.get currentDepth) >= targetDepth then
            // The exploration has exceeded the target depth but the symbolic term has not finished yet.
            // We use static analysis to remove the fixpoint terms from the term.
            // Afterwards the term is terminating (as any well typed, fixpoint-free SPCF terms is terminating), so we continue exploration (with the depth set to none).

            // Compute bounds on all sample variables
            let varBounds =
                Map.map (fun _ (d: SymbolicDistribution) -> d.Bounds) config.Fragment.SampleDistributions

            // Compute the fixpoint free term
            let fixpointFreeExpr =
                ConstraintSolver.removeAllFixpointsViaSA expr varBounds
            
            // Set the depth to None, so the path is executed until a value is reached
            queue.Enqueue(fixpointFreeExpr, config, None)
        else
            // Otherwise perform one small step reduction and increase the depth
            let res = oneStepReduction config expr
            List.iter (fun (M, c) -> queue.Enqueue(M, c, Option.map (fun x -> x + 1) currentDepth)) res

    if GlobalConstants.outputEvalDepth then
        printfn ""

    printfn "...eval completed"

    paths
