(****************************************************************************************)
(*                                                                                      *)
(*                                      Expression.fs                                   *)
(*                                                                                      *)
(****************************************************************************************)
(*                                                                                      *)
(* SPCF Expressions and Values and Basic Operations                                     *)
(*                                                                                      *)
(****************************************************************************************)

module Expression

open System

open Interval
open Util
open LinearFunction
open PrimitiveFunctions
open PrimitiveDistributions



type SymbolicValue =
    | SVVar of int
    | SVCon of double
    | SVInt of Interval
    | SVFun of FunctionSymbol * array<SymbolicValue>
    | SVTuple of SymbolicValue list
    | SVEmpty
    | SVCons of SymbolicValue * SymbolicValue

    member this.ContainsInterval =
        match this with
        | SVInt _ -> true
        | SVCon _
        | SVVar _ -> false
        | SVFun (_, args) -> Array.exists (fun (x: SymbolicValue) -> x.ContainsInterval) args
        | SVTuple elems -> List.exists (fun (x: SymbolicValue) -> x.ContainsInterval) elems
        | SVEmpty -> false
        | SVCons (head, tail) -> head.ContainsInterval || tail.ContainsInterval

    /// Extract an interval if the expression is a real or interval constant
    member this.ExtractInterval =
        match this with
        | SVInt iv -> iv
        | SVCon x -> precisely x
        | _ -> failwith "cannot extract interval"

    /// Evaluates a value when given interval values for each sample varaible
    /// Almost identical to EvalIntervalBox from below. For interval splitting Box = int array is useful.
    /// In all static usecases it is more continent to have a map instead
    member this.EvalIntervalMap(varBounds: VarBoundMap) : SymbolicValue =
        match this with
        | SVCon r -> precisely r |> SVInt
        | SVVar n -> varBounds.[n] |> SVInt
        | SVInt i -> SVInt i
        | SVFun (sym, args) ->
            let evalArgs =
                Array.map (fun (sv: SymbolicValue) -> (sv.EvalIntervalMap varBounds).ExtractInterval) args

            (PrimitiveFunctions.lookupFunctionSymbol sym)
                .ComputeInterval evalArgs
            |> SVInt
        | SVTuple elems ->
            List.map (fun (sv: SymbolicValue) -> sv.EvalIntervalMap varBounds) elems
            |> SVTuple
        | SVEmpty -> SVEmpty
        | SVCons (head, tail) -> SVCons(head.EvalIntervalMap varBounds, tail.EvalIntervalMap varBounds)

    member this.EvalBox(b: Box) : SymbolicValue =
        match this with
        | SVCon r -> precisely r |> SVInt
        | SVVar n -> b.[n] |> SVInt
        | SVInt i -> SVInt i
        | SVFun (sym, args) ->
            let evalArgs =
                Array.map (fun (sv: SymbolicValue) -> (sv.EvalBox b).ExtractInterval) args

            (PrimitiveFunctions.lookupFunctionSymbol sym)
                .ComputeInterval evalArgs
            |> SVInt
        | SVTuple elems ->
            List.map (fun (sv: SymbolicValue) -> sv.EvalBox b) elems
            |> SVTuple
        | SVEmpty -> SVEmpty
        | SVCons (head, tail) -> SVCons(head.EvalBox b, tail.EvalBox b)

    member this.UsedVars =
        match this with
        | SVVar n -> Set.singleton n
        | SVCon _
        | SVInt _
        | SVEmpty -> Set.empty
        | SVFun (_, args) -> Set.unionMany (Array.map (fun (x: SymbolicValue) -> x.UsedVars) args)
        | SVTuple elems -> Set.unionMany (List.map (fun (sv: SymbolicValue) -> sv.UsedVars) elems)
        | SVCons (head, tail) -> Set.union (head.UsedVars) (tail.UsedVars)

    /// Replaces the sample variables according to m
    member this.ReplaceVars m =
        match this with
        | SVVar i ->
            SVVar(
                match Map.tryFind i m with
                | Some j -> j
                | None -> i
            )
        | SVCon _
        | SVInt _
        | SVEmpty -> this
        | SVFun (sym, args) -> SVFun(sym, Array.map (fun (x: SymbolicValue) -> x.ReplaceVars m) args)
        | SVTuple elems ->
            List.map (fun (sv: SymbolicValue) -> sv.ReplaceVars m) elems
            |> SVTuple
        | SVCons (head, tail) -> SVCons(head.ReplaceVars m, tail.ReplaceVars m)

    /// Tries to convert the symbolic Value to a linear Function. Can fail if the value uses non-linear operations or intervals
    member this.ToLinearFunction() =
        match this with
        | SVVar i ->
            { Coefficients = Map.empty.Add(i, 1.0)
              Offset = 0.0 }
        | SVCon r -> { Coefficients = Map.empty; Offset = r }
        | SVInt _ -> failwith "cannot convert interval bounds to Linear Function"
        | SVFun (NEG, args) ->
            let lf = args.[0].ToLinearFunction()
            -lf
        | SVFun (ADD, args) ->
            let lf1 = args.[0].ToLinearFunction()
            let lf2 = args.[1].ToLinearFunction()
            lf1 + lf2
        | SVFun (SUB, args) ->
            let lf1 = args.[0].ToLinearFunction()
            let lf2 = args.[1].ToLinearFunction()
            lf1 - lf2
        | SVFun (MUL, args) ->
            let lf1 = args.[0].ToLinearFunction()
            let lf2 = args.[1].ToLinearFunction()

            if Map.forall (fun _ c -> c = 0.0) lf1.Coefficients then
                // The first argument is a constant -> Multiply second polynomial by r1
                lf2 * lf1.Offset
            elif Map.forall (fun _ c -> c = 0.0) lf2.Coefficients then
                // The second argument is a constant -> Multiply first polynomial by r2
                lf1 * lf2.Offset
            else
                failwith "Multiplication is only supported by constants"
        | _ -> failwith "Conversion Not possible"



    /// Tries to convert the symbolic Value to a Interval linear Function. Can fail if the value uses non-linear operations
    member this.ToIntervalLinearFunction() =
        match this with
        | SVVar i ->
            { Linear =
                { Coefficients = Map.empty.Add(i, 1.0)
                  Offset = 0.0 }
              AddedInterval = Interval.Zero }
        | SVCon r ->
            { Linear = { Coefficients = Map.empty; Offset = r }
              AddedInterval = Interval.Zero }
        | SVInt iv ->
            { Linear =
                { Coefficients = Map.empty
                  Offset = 0.0 }
              AddedInterval = iv }
        | SVFun (ADD, args) ->
            let lf1 = args.[0].ToIntervalLinearFunction()
            let lf2 = args.[1].ToIntervalLinearFunction()
            lf1 + lf2
        | SVFun (SUB, args) ->
            let lf1 = args.[0].ToIntervalLinearFunction()
            let lf2 = args.[1].ToIntervalLinearFunction()
            lf1 - lf2
        | SVFun (NEG, args) ->
            let lf1 = args.[0].ToIntervalLinearFunction()
            -lf1
        | SVFun (MUL, args) ->
            let lf1 = args.[0].ToIntervalLinearFunction()
            let lf2 = args.[1].ToIntervalLinearFunction()

            if Map.forall (fun _ c -> c = 0.0) lf1.Coefficients
               && lf1.AddedInterval = Interval.Zero then
                // The first argument is a constant -> Multiply second polynomial by r1
                lf2 * lf1.Offset

            elif Map.forall (fun _ c -> c = 0.0) lf2.Coefficients
                 && lf2.AddedInterval = Interval.Zero then
                // The second argument is a constant -> Multiply first polynomial by r2
                lf1 * lf2.Offset
            else
                failwith "Multiplication is only supported by constants"

        | _ -> failwith "Conversion Not possible"


    member this.TryToLinearFunction() =
        try
            this.ToLinearFunction() |> Some
        with
        | _ -> None

    member this.TryToIntervalLinearFunction() =
        try
            this.ToIntervalLinearFunction() |> Some
        with
        | _ -> None

/// A symbolic Distribution consists of the symbolic of the distribution and symbolic values of the parameters of the distribution
type SymbolicDistribution =
    { Type: DistributionSymbol
      Parameters: list<SymbolicValue> }

    member this.UsedVars =
        this.Parameters
        |> List.map (fun x -> x.UsedVars)
        |> Set.unionMany

    member this.GetPrimitiveDistribution() =
        PrimitiveDistributions.lookupDistributionSymbol this.Type

    member this.Bounds =
        let uninformativeVariableMap =
            Set.toSeq this.UsedVars
            |> Seq.map (fun x -> (x, allReals))
            |> Map.ofSeq

        let boundsOnParamters =
            this.Parameters
            |> List.map (fun x ->
                (x.EvalIntervalMap uninformativeVariableMap)
                    .ExtractInterval)

        (PrimitiveDistributions.lookupDistributionSymbol this.Type)
            .BoundsOnSamples boundsOnParamters

// Used to keep track of the original distribution of each sample variable
type VarDistMap = Map<int, SymbolicDistribution>

type SimpleType =
    | Real
    | ListTy of SimpleType
    | TupleTy of list<SimpleType>
    | Arrow of SimpleType * SimpleType

// A pointer to a location within a expression
type ExprLocation =
    | LocEpsilon
    | LocApplicationFunction of ExprLocation
    | LocApplicationArgument of ExprLocation
    | LocAbstraction of ExprLocation
    | LocFixpoint of ExprLocation
    | LocScore of ExprLocation
    | LocSample of int * ExprLocation
    | LocConditionalGuard of ExprLocation
    | LocConditionalThen of ExprLocation
    | LocConditionalElse of ExprLocation
    | LocConsLeft of ExprLocation
    | LocConsRight of ExprLocation
    | LocFoldListMatch of ExprLocation
    | LocFoldListEmpty of ExprLocation
    | LocFoldListCons of ExprLocation
    | LocPrimF of int * ExprLocation
    | LocSymPrimF of int * ExprLocation
    | LocTuple of int * ExprLocation
    | LocTupleMatchArg of ExprLocation
    | LocTupleMatchBody of ExprLocation

// The type of SPCF expression
type Expr =
    | Variable of string
    | SampleVariable of int
    | Numeral of double
    | IntervalNumeral of Interval
    | EmptyList of option<SimpleType>
    | ConsList of Expr * Expr
    // Foldlist: match list | [] -> e1 | [x|y] -> e2
    | FoldList of Expr * Expr * string * string * Expr
    | Tuple of list<Expr>
    // TupleMatch: let x_1, ..., x_n = M in N
    | TupleMatch of list<string> * Expr * Expr
    | Application of Expr * Expr
    | Abstraction of String * option<SimpleType> * Expr
    | Fixpoint of String * String * option<SimpleType * SimpleType> * Expr
    | Conditional of Expr * Expr * Expr
    | Sample of DistributionSymbol * (Expr list)
    | Score of Expr
    | PrimF of FunctionSymbol * (Expr list)
    | SymPrimF of FunctionSymbol * (Expr list)

    member this.IsValue =
        match this with
        | Variable _ 
        | Numeral _ 
        | Abstraction _ 
        | Fixpoint _ 
        | SymPrimF _ 
        | SampleVariable _ 
        | IntervalNumeral _ 
        | EmptyList _ -> true
        | ConsList (M, N) -> M.IsValue && N.IsValue
        | Tuple l -> List.forall (fun (x: Expr) -> x.IsValue) l
        | Application _ 
        | Conditional _ 
        | FoldList _ 
        | PrimF _ 
        | Sample _ 
        | TupleMatch _
        | Score _ -> false

    member this.UsedVars =
        match this with
        | SampleVariable n -> Set.singleton n
        | Application (N1, N2)
        | ConsList(N1, N2)
        | TupleMatch (_, N1, N2) -> Set.union (N1.UsedVars) (N2.UsedVars)
        | Conditional (N1, N2, N3) 
        | FoldList(N1, N2, _, _, N3) ->
            Set.unionMany [ N1.UsedVars
                            N2.UsedVars
                            N3.UsedVars ]
        | Abstraction (_, _, N)
        | Fixpoint (_, _, _, N)
        | Score N -> N.UsedVars
        | PrimF (_, args)
        | SymPrimF (_, args)
        | Sample (_, args)
        | Tuple args -> List.fold (fun s (a: Expr) -> Set.union s (a.UsedVars)) Set.empty args
        | EmptyList _ | Numeral _ | IntervalNumeral _ | Variable _ -> Set.empty

    member this.ContainsScore = 
        match this with
        | Score _ -> true
        | SampleVariable _  | EmptyList _ | IntervalNumeral _ | Numeral _ | Variable _ -> false
        | Application (N1, N2) | TupleMatch (_, N1, N2) | ConsList(N1, N2) -> (N1.ContainsScore) || (N2.ContainsScore)
        | Conditional (N1, N2, N3) | FoldList(N1, N2, _, _, N3) -> (N1.ContainsScore) || (N2.ContainsScore) || (N3.ContainsScore)
        | Abstraction (_, _, N) | Fixpoint (_, _, _, N) -> N.ContainsScore
        | PrimF (_, args)
        | SymPrimF (_, args)
        | Sample (_, args)
        | Tuple args -> List.exists (fun (x: Expr) -> x.ContainsScore) args
        
    /// Get a sub expression at a specified location
    member this.GetSubExp(loc: ExprLocation) =
        match loc, this with
        | LocEpsilon, M -> M
        | LocApplicationFunction l, Application (N, _) -> N.GetSubExp l
        | LocApplicationArgument l, Application (_, P) -> P.GetSubExp l
        | LocAbstraction l, Abstraction (_, _, N) -> N.GetSubExp l
        | LocFixpoint l, Fixpoint (_, _, _, N) -> N.GetSubExp l
        | LocScore l, Score N -> N.GetSubExp l
        | LocSample (i, l), Sample (_, args) -> args.[i].GetSubExp l
        | LocConditionalGuard l, Conditional (N, _, _) -> N.GetSubExp l
        | LocConditionalThen l, Conditional (_, P, _) -> P.GetSubExp l
        | LocConditionalElse l, Conditional (_, _, Q) -> Q.GetSubExp l
        | LocConsLeft l, ConsList (N, _) -> N.GetSubExp l
        | LocConsRight l, ConsList (_, P) -> P.GetSubExp l
        | LocFoldListMatch l, FoldList (N, _, _, _, _) -> N.GetSubExp l
        | LocFoldListEmpty l, FoldList (_, P, _, _, _) -> P.GetSubExp l
        | LocFoldListCons l, FoldList (_, _, _, _, Q) -> Q.GetSubExp l
        | LocPrimF (i, l), PrimF (_, args) -> args.[i].GetSubExp l
        | LocSymPrimF (i, l), SymPrimF (_, args) -> args.[i].GetSubExp l
        | LocTuple (i, l), Tuple args -> args.[i].GetSubExp l
        | LocTupleMatchArg l, TupleMatch (_, N, _) -> N.GetSubExp l
        | LocTupleMatchBody l, TupleMatch (_, _, N) -> N.GetSubExp l
        | _ -> failwith "Ill formed Location"

    /// Computes a map of all locations within this expression
    member this.LocationMap() =
        match this with
        | Variable _
        | Numeral _
        | IntervalNumeral _
        | EmptyList _
        | SampleVariable _ -> [ (LocEpsilon, this) ] |> Map.ofSeq
        | Application (N, P) ->
            modifyMapKey [ (LocApplicationFunction, N.LocationMap())
                           (LocApplicationArgument, P.LocationMap()) ]
            |> Map.add LocEpsilon this
        | Abstraction (_, _, N) ->
            modifyMapKey [ (LocAbstraction, N.LocationMap()) ]
            |> Map.add LocEpsilon this
        | Fixpoint (_, _, _, N) ->
            modifyMapKey [ (LocFixpoint, N.LocationMap()) ]
            |> Map.add LocEpsilon this
        | Score N ->
            modifyMapKey [ (LocScore, N.LocationMap()) ]
            |> Map.add LocEpsilon this
        | Sample (_, args) ->
            modifyMapKey (List.mapi (fun i (x: Expr) -> (fun y -> LocSample(i, y)), x.LocationMap()) args)
            |> Map.add LocEpsilon this
        | Conditional (N, P, Q) ->
            modifyMapKey [ (LocConditionalGuard, N.LocationMap())
                           (LocConditionalThen, P.LocationMap())
                           (LocConditionalElse, Q.LocationMap()) ]
            |> Map.add LocEpsilon this
        | ConsList (N, P) ->
            modifyMapKey [ (LocConsLeft, N.LocationMap())
                           (LocConsRight, P.LocationMap()) ]
            |> Map.add LocEpsilon this
        | FoldList (N, P, _, _, Q) ->
            modifyMapKey [ (LocFoldListMatch, N.LocationMap())
                           (LocFoldListEmpty, P.LocationMap())
                           (LocFoldListCons, Q.LocationMap()) ]
            |> Map.add LocEpsilon this
        | PrimF (_, args) ->
            modifyMapKey (List.mapi (fun i (x: Expr) -> (fun y -> LocPrimF(i, y)), x.LocationMap()) args)
            |> Map.add LocEpsilon this
        | SymPrimF (_, args) ->
            modifyMapKey (List.mapi (fun i (x: Expr) -> (fun y -> LocSymPrimF(i, y)), x.LocationMap()) args)
            |> Map.add LocEpsilon this
        | Tuple args ->
            modifyMapKey (List.mapi (fun i (x: Expr) -> (fun y -> LocTuple(i, y)), x.LocationMap()) args)
            |> Map.add LocEpsilon this
        | TupleMatch (_, N, P) ->
            modifyMapKey [ (LocTupleMatchArg, N.LocationMap())
                           (LocTupleMatchBody, P.LocationMap()) ]
            |> Map.add LocEpsilon this

    /// Replace a subexpression at some location with a different term
    member this.ReplaceSubExpr (loc: ExprLocation) (M: Expr) =
        match loc, this with
        | LocEpsilon, _ -> M
        | LocApplicationFunction l, Application (N, P) -> Application(N.ReplaceSubExpr l M, P)
        | LocApplicationArgument l, Application (N, P) -> Application(N, P.ReplaceSubExpr l M)
        | LocAbstraction l, Abstraction (x, t, N) -> Abstraction(x, t, N.ReplaceSubExpr l M)
        | LocFixpoint l, Fixpoint (f, x, t, N) -> Fixpoint(f, x, t, N.ReplaceSubExpr l M)
        | LocScore l, Score N -> Score(N.ReplaceSubExpr l M)
        | LocSample (i, l), Sample (dist, args) ->
            Sample(
                dist,
                List.mapi
                    (fun j (x: Expr) ->
                        if i = j then
                            x.ReplaceSubExpr l M
                        else
                            x)
                    args
            )
        | LocConditionalGuard l, Conditional (N, P, Q) -> Conditional(N.ReplaceSubExpr l M, P, Q)
        | LocConditionalThen l, Conditional (N, P, Q) -> Conditional(N, P.ReplaceSubExpr l M, Q)
        | LocConditionalElse l, Conditional (N, P, Q) -> Conditional(N, P, Q.ReplaceSubExpr l M)
        | LocConsLeft l, ConsList (N, P) -> ConsList(N.ReplaceSubExpr l M, P)
        | LocConsRight l, ConsList (N, P) -> ConsList(N, P.ReplaceSubExpr l M)
        | LocFoldListMatch l, FoldList (N, P, x, y, Q) -> FoldList(N.ReplaceSubExpr l M, P, x, y, Q)
        | LocFoldListEmpty l, FoldList (N, P, x, y, Q) -> FoldList(N, P.ReplaceSubExpr l M, x, y, Q)
        | LocFoldListCons l, FoldList (N, P, x, y, Q) -> FoldList(N, P, x, y, Q.ReplaceSubExpr l M)
        | LocPrimF (i, l), PrimF (name, args) ->
            PrimF(
                name,
                List.mapi
                    (fun j (x: Expr) ->
                        if i = j then
                            x.ReplaceSubExpr l M
                        else
                            x)
                    args
            )
        | LocSymPrimF (i, l), SymPrimF (name, args) ->
            SymPrimF(
                name,
                List.mapi
                    (fun j (x: Expr) ->
                        if i = j then
                            x.ReplaceSubExpr l M
                        else
                            x)
                    args
            )
        | LocTuple (i, l), Tuple args ->
            Tuple(
                List.mapi
                    (fun j (x: Expr) ->
                        if i = j then
                            x.ReplaceSubExpr l M
                        else
                            x)
                    args
            )
        | LocTupleMatchArg l, TupleMatch (vars, N, P) -> TupleMatch(vars, N.ReplaceSubExpr l M, P)
        | LocTupleMatchBody l, TupleMatch (vars, N, P) -> TupleMatch(vars, N, P.ReplaceSubExpr l M)
        | _ -> failwith "Ill formed Location"

    /// Standard (capture avodining) substitution of variables with expressions 
    member this.ParaSubst(l: Map<String, Expr>) =
        match this with
        | Numeral v -> Numeral v
        | IntervalNumeral i -> IntervalNumeral i
        | Variable y ->
            if Map.containsKey y l then
                l.[y]
            else
                Variable y
        | Application (M1, M2) -> Application(M1.ParaSubst l, M2.ParaSubst l)
        | Abstraction (y, t, M1) -> Abstraction(y, t, M1.ParaSubst(Map.remove y l))
        | Fixpoint (f, y, t, body) ->
            let newSubst = l |> Map.remove f |> Map.remove y
            Fixpoint(f, y, t, body.ParaSubst newSubst)
        | Sample (dist, args) -> Sample(dist, List.map (fun (a: Expr) -> a.ParaSubst l) args)
        | Conditional (M1, M2, M3) -> Conditional(M1.ParaSubst l, M2.ParaSubst l, M3.ParaSubst l)
        | Score (M1) -> Score(M1.ParaSubst l)
        | SampleVariable n -> SampleVariable n
        | PrimF (name, args) -> PrimF(name, List.map (fun (a: Expr) -> a.ParaSubst l) args)
        | SymPrimF (name, args) -> SymPrimF(name, List.map (fun (a: Expr) -> a.ParaSubst l) args)
        | EmptyList t -> EmptyList t
        | ConsList (first, rest) -> ConsList(first.ParaSubst l, rest.ParaSubst l)
        | FoldList (scrutinee, empty, y, ys, cons) ->
            let scrutinee' = scrutinee.ParaSubst l
            let empty' = empty.ParaSubst l

            let cons' =
                let newSubst = l |> Map.remove y |> Map.remove ys
                cons.ParaSubst newSubst

            FoldList(scrutinee', empty', y, ys, cons')
        | Tuple args -> Tuple(List.map (fun (a: Expr) -> a.ParaSubst l) args)
        | TupleMatch (vars, M1, M2) ->
            let args = M1.ParaSubst l

            let newSubst =
                List.fold (fun s x -> Map.remove x s) l vars

            let body = M2.ParaSubst newSubst
            TupleMatch(vars, args, body)

    member this.Subst x M =
        let l = Map.empty |> Map.add x M
        this.ParaSubst l

    /// Tries to convert the expression to a symbolic value. Fails if the expression is not already a value
    member this.ToSymbolicValue() =
        match this with
        | Numeral r -> SVCon r
        | SampleVariable n -> SVVar n
        | IntervalNumeral i -> SVInt i
        | SymPrimF (sym, args) ->
            SVFun(
                sym,
                List.map (fun (x: Expr) -> x.ToSymbolicValue()) args
                |> List.toArray
            )
        | EmptyList _ -> SVEmpty
        | ConsList (head, tail) -> SVCons(head.ToSymbolicValue(), tail.ToSymbolicValue())
        | Tuple elems ->
            List.map (fun (e: Expr) -> e.ToSymbolicValue()) elems
            |> SVTuple
        | _ -> failwith $"Non-supported value: {this}"


/// Represents a guard in the program execution, i.e., a constraints of the form V \bowtie t for \bowtie \in {<, ,=, =, >, .=}
type Guard =
    { Value: SymbolicValue
      Com: Compare
      Threshold: double }

    member this.UsedVars = this.Value.UsedVars

    member this.ToLinearInequality() =
        { LinearInequality.Function = this.Value.ToLinearFunction()
          Com = this.Com
          Threshold = this.Threshold }

    member this.ToIntervalLinearInequality() =
        { Function = this.Value.ToIntervalLinearFunction()
          Com = this.Com
          Threshold = this.Threshold }

/// Converts a SPCF expression to an anglican expression
let rec toAnglican (term: Expr) =
    let varsToList (xs: string list) =
        let elems =
            xs |> List.fold (fun s x -> s + $"{x} ") ""

        $"[ {elems}]"

    match term with
    | Variable v -> v
    | Numeral d -> $"{d}"
    | EmptyList _ -> "[]"
    | ConsList (x, xs) -> $"(cons {toAnglican x} {toAnglican xs})"
    // Foldlist: match list | [] -> e1 | [x|y] -> e2
    | FoldList (scrutinee, emptyCase, head, tail, consCase) ->
        let scrutineeTranslated = toAnglican scrutinee
        let emptyTranslated = toAnglican emptyCase
        let consTranslated = toAnglican consCase
        $"(let [[{head} & {tail}] {scrutineeTranslated}] (if (nil? {head}) {emptyTranslated} {consTranslated}))"
    | Tuple (xs) ->
        let elems =
            xs
            |> List.fold (fun s p -> s + $" {toAnglican p}") ""

        $"[ {elems}]"
    // TupleMatch: let x_1, ..., x_n = M in N
    | TupleMatch (xs, e1, e2) ->
        let e1Translated = toAnglican e1
        let e2Translated = toAnglican e2
        $"(let [{varsToList xs} {e1Translated}] {e2Translated})"
    | Application (e1, e2) ->
        let e1Translated = toAnglican e1
        let e2Translated = toAnglican e2
        $"({e1Translated} {e2Translated})"
    | Abstraction (x, _, e) ->
        let eTranslated = toAnglican e
        let var = if x = "" then "_unused" else x
        $"(fn [{var}] {eTranslated})"
    | Fixpoint (f, x, _, e) ->
        let eTranslated = toAnglican e
        $"(FIX (fn [{f}] (fn [{x}] {eTranslated})))"
    | Conditional (cond, thenBranch, elseBranch) ->
        let condTranslated = toAnglican cond
        let thenTranslated = toAnglican thenBranch
        let elseTranslated = toAnglican elseBranch
        $"(if (<= {condTranslated} 0) {thenTranslated} {elseTranslated})"
    | Sample (distSym, exprs) ->
        let name = (lookupDistributionSymbol distSym).Name

        let name =
            if name = "uniform" then
                "uniform-continuous"
            else
                name

        let paramsTranslated =
            exprs
            |> List.fold (fun s p -> s + $" {toAnglican p}") ""

        $"(recordtrace (sample ({name}{paramsTranslated})))"
    | Score e ->
        let eTranslated = toAnglican e
        $"(scorevalue {eTranslated})"
    | PrimF (f, exprs) ->
        let name = (lookupFunctionSymbol f).Name

        let name =
            match name with
            | "neg" -> "-"
            | "add" -> "+"
            | "sub" -> "-"
            | "mul" -> "*"
            | "div" -> "/"
            | _ -> name

        let paramsTranslated =
            exprs
            |> List.fold (fun s p -> s + $" {toAnglican p}") ""

        $"({name}{paramsTranslated})"
    | SampleVariable _
    | IntervalNumeral _
    | SymPrimF _ -> failwith "not supported"

/// Translates a SPCF expression to a anglican query
let translateToAnglican (term: Expr) (name: string) =
    $"(ns {name}
  (:require [gorilla-plot.core :as plot])
  (:use [anglican core emit runtime
         [state :only [get-predicts get-log-weight get-result]]]))\n"
    + "(defm FIX [f] (fn [x] ((f (FIX f)) x)))\n"
    + "(defm scorevalue [s] (observe (exponential s) 0) s)"
    + "(defm recordtrace [s] (predict s) s) ;; we abuse `predict` to record the trace\n"
    + "(defn pdfnormal [mean var x] (/ (exp (- (/ (* (- x mean) (- x mean)) (* 2 var)))) (Math/sqrt (* 2 Math/PI var))))\n"
    + $"(with-primitive-procedures [pdfnormal] (defquery program\n      {toAnglican term}\n))\n"
    + "(let [warmup 100
      count 100
      thinning 50
      rawsamples (take-nth thinning (take (* count thinning) (drop warmup (doquery :lmh program []))))
      ;; If you use an importance sampling based method (e.g. particle filters),
      ;; make sure to postprocess the samples to take :log-weight into account.
      samples (map (fn [s] [(:result s) (into [] (map second (:predicts s)))]) rawsamples)
      values (map first samples)]
  (println values)
  (doall (map (fn [s] (println (clojure.string/join \" \" [(str (first s)) (str (second s))]))) samples))
  (plot/histogram values :bins 20 :normalize :probability))\n"
