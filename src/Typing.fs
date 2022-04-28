(****************************************************************************************)
(*                                                                                      *)
(*                                      Typing.fs                                       *)
(*                                                                                      *)
(****************************************************************************************)
(*                                                                                      *)
(* Simple Constraint-based Type System For SPCF                                         *)
(*                                                                                      *)
(****************************************************************************************)
module Typing

open System
open Expression

// A type for types that includes type variables that can be instantiated
type private TyVar = int
type private Type =
    | Var of TyVar
    | Real
    | ListTy of Type
    | TupleTy of list<Type>
    | Arrow of Type * Type

    member this.Subst i t =
        match this with
        | Var j -> if i = j then t else Var j
        | Real -> Real
        | ListTy it -> ListTy (it.Subst i t)
        | TupleTy it -> TupleTy (List.map (fun (x : Type) -> x.Subst i t) it)
        | Arrow (t1, t2) -> Arrow(t1.Subst i t, t2.Subst i t)

    member this.ApplySubst (s: Map<TyVar, Type>) =
        match this with
            | Var i as t ->
                match Map.tryFind i s with
                | Some tau -> tau
                | None -> t
            | Real -> Real
            | ListTy t -> ListTy (t.ApplySubst s)
            | TupleTy tlist -> TupleTy (List.map (fun (x : Type) -> x.ApplySubst s) tlist)
            | Arrow (t1, t2) -> Arrow(t1.ApplySubst s, t2.ApplySubst s)

    member this.AsSimpleType =
        match this with
            | Var _ -> failwith "Invalid Call"
            | Real -> SimpleType.Real
            | ListTy t -> SimpleType.ListTy t.AsSimpleType
            | TupleTy tlist -> SimpleType.TupleTy (List.map (fun (x : Type) -> x.AsSimpleType) tlist)
            | Arrow (t1, t2) -> SimpleType.Arrow(t1.AsSimpleType, t2.AsSimpleType)
    
    member this.FreeVars =
        match this with
            | Var i -> Set.singleton i
            | Real -> Set.empty
            | ListTy t -> t.FreeVars
            | TupleTy tlist -> 
                tlist
                |> List.map (fun (x:Type) -> x.FreeVars)
                |> Set.unionMany
            | Arrow (t1, t2) -> Set.union t1.FreeVars t2.FreeVars


type private TypeEnv = Map<String, Type>

type private Subst = Map<TyVar, Type>

// A type constraints (t, l) requires both types to be identical. The constraints will be solved by unifying both types.
type private TypeConstraint = Type * Type

let private epsilonMap (t : 'a) =
    Map.empty
    |> Map.add LocEpsilon t

exception private TypeConstraintEx of string
///Infers the constraints as type constraints that a typing derivation needs to satisfy. These rules are standard
let private inferTypeConstraints (M: Expr) =
    
    let rec inferTypeConstraintsRec (gamma: TypeEnv) gen (M: Expr) : Map<ExprLocation, Type> * list<TypeConstraint> =
        match M with
        | Variable x ->
            match Map.tryFind x gamma with
            | Some t -> (epsilonMap t, [])
            | None -> raise (TypeConstraintEx ("Term is not closed for variable: " + x))
        | Abstraction (var, _, N) ->
            let tv = Var <| gen ()
            let gamma' = 
                gamma
                |> Map.add var tv
            let (t, constraints) = inferTypeConstraintsRec gamma' gen N
            let newT =
                Util.modifyMapKey [(LocAbstraction, t)] 
                |> Map.add LocEpsilon (Arrow(tv, t.[LocEpsilon]))
            newT, constraints
        | Application (N, P) ->
            let tv = Var <| gen ()
            let (tN, cN) = inferTypeConstraintsRec gamma gen N
            let (tP, cP) = inferTypeConstraintsRec gamma gen P
            let newT =
                Util.modifyMapKey [(LocApplicationFunction, tN); (LocApplicationArgument, tP)] 
                |> Map.add LocEpsilon tv
            let constraints =
                (tN.[LocEpsilon], Arrow(tP.[LocEpsilon], tv))
                :: cN @ cP
            (newT, constraints)
        | Fixpoint (fixvar, var, _, N) ->
            let tv = Var <| gen ()
            let tvret = Var <| gen ()
            let gamma' =
                gamma
                |> Map.add var tv
                |> Map.add fixvar (Arrow(tv, tvret))
            let (tN, cN) = inferTypeConstraintsRec gamma' gen N
            let newT =
                Util.modifyMapKey [(LocFixpoint, tN)] 
                |> Map.add LocEpsilon (Arrow(tv, tvret))
            let constraints =
                (tvret, tN.[LocEpsilon])::cN
            (newT, constraints)
        | Numeral _ | IntervalNumeral _ -> (epsilonMap Real, [])
        | EmptyList _ ->
            let tv = Var <| gen ()
            (epsilonMap (ListTy tv), [])
        | ConsList (N, P) ->
            let (tN, cN) = inferTypeConstraintsRec gamma gen N
            let (tP, cP) = inferTypeConstraintsRec gamma gen P
            let constraints = 
                (ListTy tN.[LocEpsilon], tP.[LocEpsilon])
                :: cN @ cP
            let newT =
                Util.modifyMapKey [(LocConsLeft, tN); (LocConsRight, tP)] 
                |> Map.add LocEpsilon tP.[LocEpsilon]
            (newT, constraints)
        | FoldList (N, P, x, xs, Q) ->
            let (tN, cN) = inferTypeConstraintsRec gamma gen N
            let (tP, cP) = inferTypeConstraintsRec gamma gen P
            let tv = Var <| gen ()
            let gamma' = gamma |> Map.add x tv |> Map.add xs (ListTy tv)
            let (tQ, cQ) = inferTypeConstraintsRec gamma' gen Q
            let constraints = 
                [(tN.[LocEpsilon], ListTy tv); (tP.[LocEpsilon], tQ.[LocEpsilon])]
                @ cN @ cP @ cQ
            let newT =
                Util.modifyMapKey [(LocFoldListMatch, tN); (LocFoldListEmpty, tP); (LocFoldListCons, tQ)] 
                |> Map.add LocEpsilon tP.[LocEpsilon]
            (newT, constraints)
        | Sample (_, args) -> 
            let res =
                List.map (inferTypeConstraintsRec gamma gen) args
            let newT =
                Util.modifyMapKey
                    (List.mapi (fun i (x, _) -> ((fun y -> LocSample(i, y)), x)) res)
                    |> Map.add LocEpsilon Real
            let constraints =
                List.collect (fun (x : Map<ExprLocation,Type>, y) -> (x.[LocEpsilon], Real)::y) res
            (newT, constraints)
        | Conditional (N, P, Q) ->
            let (tN, cN) = inferTypeConstraintsRec gamma gen N
            let (tP, cP) = inferTypeConstraintsRec gamma gen P
            let (tQ, cQ) = inferTypeConstraintsRec gamma gen Q
            let constraints =
                [(tN.[LocEpsilon], Real); (tP.[LocEpsilon], tQ.[LocEpsilon])]
                @ cN @ cP @ cQ
            let newT =
                Util.modifyMapKey [(LocConditionalGuard, tN); (LocConditionalThen, tP); (LocConditionalElse, tQ)]
                |> Map.add LocEpsilon tP.[LocEpsilon]
            (newT, constraints)
        | Score N ->
            let (tN, cN) = inferTypeConstraintsRec gamma gen N
            let newT =
                Util.modifyMapKey [(LocScore, tN)] 
                |> Map.add LocEpsilon Real
            let constraints =
                (tN.[LocEpsilon], Real) :: cN
            (newT, constraints)
        | PrimF (_, args) ->
            let res =
                List.map (inferTypeConstraintsRec gamma gen) args
            let newT =
                Util.modifyMapKey
                    (List.mapi (fun i (x, _) -> ((fun y -> LocPrimF(i, y)), x)) res)
                    |> Map.add LocEpsilon Real
            let constraints =
                List.collect (fun (x : Map<ExprLocation,Type>, y) -> (x.[LocEpsilon], Real)::y) res
            (newT, constraints)
        | Tuple args ->
            let res =
                List.map (inferTypeConstraintsRec gamma gen) args
            let componentTypes = 
                res
                |> List.map (fun (x, _) -> x.[LocEpsilon])
            let newT =
                Util.modifyMapKey
                    (List.mapi (fun i (x, _) -> ((fun y -> LocTuple(i, y)), x)) res)
                    |> Map.add LocEpsilon (TupleTy componentTypes)
            let constraints =
                List.collect (fun (_, y) -> y) res
            (newT, constraints)
        
        | TupleMatch (vars, N, P) ->   
            let (tN, cN) = inferTypeConstraintsRec gamma gen N
            let freshTupleTv =   
                List.init (List.length vars) (fun _ -> Var <| gen ())
            let gamma' = 
                List.zip vars freshTupleTv
                |> List.fold (fun s (x, t) -> Map.add x t s) gamma
            let (tP, cP) = inferTypeConstraintsRec gamma' gen P    
            let newT =
                Util.modifyMapKey [(LocTupleMatchArg, tN); (LocTupleMatchBody, tP)] 
                |> Map.add LocEpsilon (tP.[LocEpsilon])
            let constraints =
                (tN.[LocEpsilon], TupleTy freshTupleTv) ::
                cN @
                cP       
            (newT, constraints)

        | SampleVariable _
        | SymPrimF _
            -> raise (TypeConstraintEx ("invalid expression"))
    
     // Generator for fresh type variables
    let gen =
        let a = ref 0
        fun () ->
            let t = !a
            a := !a + 1
            t

    try 
        Ok (inferTypeConstraintsRec Map.empty gen M)
    with 
        | TypeConstraintEx s  -> Error s
        


/// Given a set of type constraints, we try to find a solution (a map from type vars to closed types) that fulfill it
/// The function does this by repeated unification of types, whenever both types mismatch in the current context. 
let private solveConstraints (constraints : list<TypeConstraint>) =
    let rec solveConstraintsRec constraints (A : Map<TyVar, Type>) =
        match constraints with
            | [] -> Some A 
            | x::xs ->
                match x with
                    | (Real, Real) -> solveConstraintsRec xs A
                    | (Var i, t)
                    | (t, Var i) ->
                        if t = Var i then
                            // They are identical, so the constraint is resolved, continue with xs
                            solveConstraintsRec xs A
                        else
                            if Set.contains i t.FreeVars then
                                // t contains i as a variables and both are not equal. We cannot unify those types, the term is not typeable
                                None
                            else
                                // Substitute t for i and thereby unify both types
                                let newA = 
                                    A
                                    |> Map.map (fun _ x -> x.Subst i t)
                                    |> Map.add i t
                                let newXs = (List.map ( fun (t1 : Type, t2 : Type) -> t1.Subst i t, t2.Subst i t) xs)
                                solveConstraintsRec newXs newA
                    | (Arrow (t1, t2), Arrow (l1, l2)) ->
                        // Recurse on the smaller types, i.e., if Arrow (t1, t2)=  Arrow (l1, l2) then t1 = l1 and t2 = l2
                        solveConstraintsRec ((t1, l1) :: (t2, l2) :: xs) A
                    | (ListTy t1, ListTy t2) ->
                        // Recurse on the smaller types
                        solveConstraintsRec ((t1, t2) :: xs) A 
                    | TupleTy t1, TupleTy t2 ->
                        // Recurse on the smaller types
                        let zipped = List.zip t1 t2
                        solveConstraintsRec (zipped @ xs) A
                    | _ ->
                        // Both types have a different form at the top level, they cannot be unified
                        None

    solveConstraintsRec constraints Map.empty


/// Checks if a program can be typed
let isWellTypedConstraints (M: Expr) =
   

    match inferTypeConstraints  M with 
        | Error _ -> false
        | Ok (_, constraints) -> 
            let sol = solveConstraints constraints

            match sol with 
                | Some _ -> true 
                | None -> false

/// Checks if a Program can be typed and, if so, annotates it (i.e., removes the optional type arguments with actual types)
let checkAndAnnotateType (expr: Expr) =

    match inferTypeConstraints expr with
        | Error _ -> None
        | Ok (locMap, constraints) -> 
            match solveConstraints constraints with 
                | None -> None
                | Some sol -> 
                    if (locMap.[LocEpsilon].ApplySubst sol).AsSimpleType <> SimpleType.Real then 
                        // The top-level type must be Real
                        None
                    else 
                        // Filter out all positions in expr that need annotation (these are abstraction, fixpoints and emptylists). 
                        let typeAnnotationLocs =
                            expr.LocationMap() 
                            |> Map.toSeq
                            |> Seq.filter (fun (_, M) -> 
                                match M with
                                    | Abstraction _ | Fixpoint _ | EmptyList _ -> true
                                    | _ -> false
                                )
                            |> Seq.map fst

                        let mutable term = expr

                        // For each location that needs annotation, insert the type that is obtained as the solution of the typing constraints at that location
                        for loc in typeAnnotationLocs do
                            let solType = (locMap.[loc].ApplySubst sol).AsSimpleType
                            match term.GetSubExp loc with
                                | Abstraction(x, _, M) -> 
                                    match solType with
                                        | SimpleType.Arrow (t, _) -> 
                                            term <- term.ReplaceSubExpr loc (Abstraction(x, Some t, M))
                                        | _ -> failwith "Impossible"
                                | Fixpoint(f, x, _, M) -> 
                                    match solType with
                                        | SimpleType.Arrow (t1, t2) -> 
                                            term <- term.ReplaceSubExpr loc (Fixpoint(f, x, Some (t1, t2), M))
                                        | _ -> failwith "Impossible"
                                | EmptyList _ -> 
                                    match solType with
                                        | SimpleType.ListTy t -> 
                                            term <- term.ReplaceSubExpr loc (EmptyList (Some t))
                                        | _ -> failwith "Impossible"
                                | _ -> ()

                        Some term
   