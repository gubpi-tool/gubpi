(****************************************************************************************)
(*                                                                                      *)
(*                                      ConstraintGeneration.fs                         *)
(*                                                                                      *)
(****************************************************************************************)
(*                                                                                      *)
(* Generates the typing constraints for the interval-type system                        *)
(*                                                                                      *)
(****************************************************************************************)

module ConstraintGeneration


open Interval
open Util
open PrimitiveFunctions
open PrimitiveDistributions
open Expression

type SymType =
    | SymReal of int
    | SymListTy of SymType
    | SymTupleTy of list<SymType>
    | SymArrow of SymType * SymTypeWeight

and SymTypeWeight = SymType * int

let rec private baseType =
    function
    | SymReal _ -> SimpleType.Real
    | SymListTy t -> SimpleType.ListTy (baseType t)
    | SymTupleTy tl -> SimpleType.TupleTy (List.map baseType tl)
    | SymArrow (t1, t2) -> SimpleType.Arrow(baseType t1, baseTypeWeight t2)

and private baseTypeWeight (t, _) = baseType t

type Constraint =
    | Equality of int * Interval
    | Subset of int * int
    | FunctionResult of int * FunctionSymbol * list<int>
    | DistributionResult of int * DistributionSymbol * list<int>

    member this.UsedVars =
        match this with
        | Equality (i, _) -> Set.singleton (i)
        | Subset (i, j) -> Set.add i (Set.singleton j)
        | FunctionResult (i, _, args) -> List.fold (fun s i -> Set.add i s) (Set.singleton (i)) args
        | DistributionResult (i, _, args) -> List.fold (fun s i -> Set.add i s) (Set.singleton (i)) args

    member this.TargetVar =
        match this with
        | Equality (i, _) -> i
        | Subset (_, i) -> i
        | FunctionResult (i, _, _) -> i
        | DistributionResult (i, _, _) -> i

    member this.SourceVars =
        match this with
        | Equality _ -> Set.empty
        | Subset (i, _) -> Set.singleton i
        | FunctionResult (_, _, args) -> set args
        | DistributionResult (_, _, args) -> set args

/// Generates the constraints need to ensure that two symbolic types are in the subset relation
let rec private reduceSubtypesToConstraints symTy1 symTy2 =
    match symTy1, symTy2 with
    | SymReal i, SymReal j -> [ Subset(i, j) ]
    | SymListTy t1, SymListTy t2 -> reduceSubtypesToConstraints t1 t2
    | SymTupleTy tl1, SymTupleTy tl2 ->
        List.zip tl1 tl2
        |> List.collect (fun (x, y) -> reduceSubtypesToConstraints x y)
    | SymArrow (k1d, k1r), SymArrow (k2d, k2r) -> reduceSubtypesToConstraints k2d k1d @ reduceWeightedSubtypesToConstraints k1r k2r
    | _ -> failwith "Only on types with same base"

and private reduceWeightedSubtypesToConstraints (t1, nu1) (t2, nu2) = Subset(nu1, nu2) :: reduceSubtypesToConstraints t1 t2

type FreshIntGenerator = unit -> int

let rec private generateSymbolicType (gen: FreshIntGenerator) (t: SimpleType) =
    match t with
    | SimpleType.Real -> SymReal <| gen()
    | SimpleType.ListTy t -> SymListTy (generateSymbolicType gen t)
    | SimpleType.TupleTy tl -> SymTupleTy(List.map (generateSymbolicType gen) tl)
    | SimpleType.Arrow (t1, t2) -> SymArrow(generateSymbolicType gen t1, generateWeightedSymoblicType gen t2)

and generateWeightedSymoblicType gen (t: SimpleType) =
    let nu = gen ()
    (generateSymbolicType gen t, nu)


/// Given a list of weight, generates a constraints for the multiplication of all those weights
/// Returns the list of constraints and the id for the result
let rec private multiplyWeights gen (weights: list<int>) : (int * list<Constraint>) =
    match weights with
    | [] ->
        let weight = gen ()
        (weight, [ Equality(weight, Interval.One) ])
    | [ weight ] -> (weight, [])
    | [ weight1; weight2 ] ->
        let weight = gen ()
        (weight, [ FunctionResult(weight, MUL, [ weight1; weight2 ]) ])
    | first :: rest ->
        let (temp, restConstraints) = multiplyWeights gen rest
        let weight = gen ()
        (weight, FunctionResult(weight, MUL, [ first; temp ]) :: restConstraints)

/// Creates the empty Map for a type
let private epsilonMap (t : SymTypeWeight) =
    Map.empty.Add(LocEpsilon, t)

/// Generates the constraints by following the syntax guided rules for the interval-type system
/// Instead of only maintaining the type of the term at its top level, we maintain a map that gives the type for each subexpression of the term
let rec private generateConstraintRec (context: Map<string, SymType>) (M: Expr) (bounds: VarBoundMap) (gen: FreshIntGenerator) : Map<ExprLocation, SymTypeWeight> * list<Constraint> =
    match M with
    | Variable var ->
        let symTy = Map.find var context
        let weightVar = gen()

        let cT = (symTy, weightVar)
        (epsilonMap cT, [ Equality(weightVar, Interval.One) ])
    | Numeral r ->
        let valueVar = gen ()
        let weightVar = gen ()

        let cT = (SymReal valueVar, weightVar)
        (epsilonMap cT,
         [ Equality(valueVar, precisely r)
           Equality(weightVar, Interval.One) ])
    | IntervalNumeral iv ->
        let valueVar = gen ()
        let weightVar = gen ()

        let cT = (SymReal valueVar, weightVar)

        (epsilonMap cT,
         [ Equality(valueVar, iv)
           Equality(weightVar, Interval.One) ])
    | SampleVariable i ->
        let iv = bounds.[i]
        let valueVar = gen ()
        let weightVar = gen ()

        let cT = (SymReal valueVar, weightVar)
        (epsilonMap cT, [ Equality(valueVar, iv); Equality(weightVar, Interval.One) ])
    | Abstraction (x, xType, body) ->
        match xType with
        | None -> failwith "No Type given"
        | Some domainType ->
            // Create a fresh variable for the type of the argument
            let symTy = generateSymbolicType gen domainType
            let weightVar = gen ()

            // Type the body of the abstraction
            let (locType, constraints) =
                generateConstraintRec (Map.add x symTy context) body bounds gen

            let functionType = (SymArrow(symTy, locType.[LocEpsilon]), weightVar)

            let newLocType =
                Util.modifyMapKey [LocAbstraction, locType]
                |> Map.add LocEpsilon functionType

            (newLocType, Equality(weightVar, Interval.One) :: constraints)
    | Fixpoint (f, x, t, body) ->
        match t with
        | None -> failwith "No Type given"
        | Some (xType, bodyType) ->
            // Create a fresh variable for the type of the argument and the return value of the function
            let xSymTy = generateSymbolicType gen xType
            let bodySymTy = generateWeightedSymoblicType gen bodyType
            let weightVar = gen ()

            let context' =
                context
                |> Map.add f (SymArrow(xSymTy, bodySymTy))
                |> Map.add x xSymTy

            let (locType, bodyConstraints) = generateConstraintRec context' body bounds gen

            // Add a constraint that guarantees that the argument can be passed to the recursive function
            let subtypeConstraints = reduceWeightedSubtypesToConstraints locType.[LocEpsilon] bodySymTy

            let cT = (SymArrow(xSymTy, locType.[LocEpsilon]), weightVar)

            let newLocType =
                Util.modifyMapKey [LocFixpoint, locType]
                |> Map.add LocEpsilon cT

            (newLocType, Equality(weightVar, Interval.One) :: bodyConstraints @ subtypeConstraints)
    | Application (N, P) ->

        let (locTypeN, constraintsN) = generateConstraintRec context N bounds gen

        let (locTypeP, constraintsP) = generateConstraintRec context P bounds gen

        match locTypeN.[LocEpsilon] with
            | (SymArrow (domainSymTyN, (resultSymTyN, innerWeightN)), weightN) ->

                let (symTyP, weightP) = locTypeP.[LocEpsilon]
                
                // Ensure that the argument fits the type of the function
                let subtypeConstraints = reduceSubtypesToConstraints symTyP domainSymTyN

                // multiply all three weights
                let resultWeight, weightConstraints = multiplyWeights gen [weightN; weightP; innerWeightN]

                let cT = (resultSymTyN, resultWeight)

                let newLocType =
                    Util.modifyMapKey [(LocApplicationFunction, locTypeN); (LocApplicationArgument, locTypeP)]
                    |> Map.add LocEpsilon cT

                (newLocType, constraintsN @ constraintsP @ subtypeConstraints @ weightConstraints)

            | _ -> failwith "not well typed"
    | Conditional (N, P, Q) ->

        let locTypeN, constraintsN = generateConstraintRec context N bounds gen
        let locTypeP, constraintsP = generateConstraintRec context P bounds gen
        let locTypeQ, constraintsQ = generateConstraintRec context Q bounds gen

        let (_, weightN) = locTypeN.[LocEpsilon]
        let (symTypeP, weightP) = locTypeP.[LocEpsilon]
        let (symTypeQ, weightQ) = locTypeQ.[LocEpsilon]

        let weight, weightConstraints =
            let branchWeight = gen ()
            let weightConstraints1 = [ Subset(weightP, branchWeight); Subset(weightQ, branchWeight) ]
            let tW, weightConstraints2 = multiplyWeights gen [weightN; branchWeight]
            tW, weightConstraints1 @ weightConstraints2

        // Generate new symbolic type for the return type (base type of P and Q agrees)
        let resultSymTy = generateSymbolicType gen (baseType symTypeP)

        // Ensure that both branches are subtypes of the result of the conditional (resultSymTy)
        let subtypeConstraints =
            reduceSubtypesToConstraints symTypeP resultSymTy
            @ reduceSubtypesToConstraints symTypeQ resultSymTy

        let cT = (resultSymTy, weight)

        let newLocType =
            Util.modifyMapKey [LocConditionalGuard, locTypeN; LocConditionalThen, locTypeP; LocConditionalElse, locTypeQ]
            |> Map.add LocEpsilon cT

        let constraints = weightConstraints @ constraintsN @ constraintsP @ constraintsQ @ subtypeConstraints

        (newLocType, constraints)
    | Sample (dist, args) ->
        // Generate constraints for all parameters of the distribution
        let typedArgs =
            List.map (fun arg -> generateConstraintRec context arg bounds gen) args

        let argConstraints = List.collect snd typedArgs

        let argVars =
            typedArgs
            |> List.map
                (fun (m , _) ->
                    match m.[LocEpsilon] with
                    | SymReal i, _ -> i
                    | _ -> failwith "Not well typed")
        let argWeights =
            typedArgs
            |> List.map
                (fun (m , _) ->
                    let _, w = m.[LocEpsilon]
                    w)

        let result = gen ()
        let (weight, weightConstraints) = multiplyWeights gen argWeights

        let cT = (SymReal result, weight)

        let newLocType =
            Util.modifyMapKey (
                List.mapi
                    (fun i (m, _) -> ((fun y -> LocSample(i, y)), m) )
                    typedArgs
            )
            |> Map.add LocEpsilon cT

        (newLocType, DistributionResult(result, dist, argVars) :: (weightConstraints @ argConstraints))
    | Score N ->
        let locTypeN, constraintsN = generateConstraintRec context N bounds gen
        let symN, weightN = locTypeN.[LocEpsilon]

        match symN with
        | SymReal factorVar ->
            let resultWeight = gen ()

            let cT = symN, resultWeight

            let newLocType =
                Util.modifyMapKey [LocScore, locTypeN]
                |> Map.add LocEpsilon cT

            (newLocType, FunctionResult(resultWeight, MUL, [ factorVar; weightN ]) :: constraintsN)
        | _ -> failwith "impossible: score expression must be base type"
    | EmptyList t ->
        match t with
        | None -> failwith "No Type given"
        | Some baseType ->
            let valueType = generateSymbolicType gen baseType
            let weightVar = gen ()

            let cT = (SymListTy valueType, weightVar)
            // The type itself is unconstrained
            (epsilonMap cT,
             [ Equality(weightVar, Interval.One) ])

    | ConsList (N, P) ->
        let locTypeN, constraintsN = generateConstraintRec context N bounds gen
        let locTypeP, constraintsP = generateConstraintRec context P bounds gen

        let (symTypeN, weightN) = locTypeN.[LocEpsilon]
        let (symTypeP, weightP) = locTypeP.[LocEpsilon]

        // Multiply the weight of both parts
        let totalWeight, weightConstraints = multiplyWeights gen [weightN; weightP]

        // Generate a new Type that subsumes both. This is a list type
        let freshType = generateSymbolicType gen (baseType symTypeP)

        let cT = (freshType, totalWeight)

        let newLocType =
            Util.modifyMapKey [LocConsLeft, locTypeN; LocConsRight, locTypeP]
            |> Map.add LocEpsilon cT

        // Ensure that both parts have the same parent (freshType)
        let subConstraintsN = reduceSubtypesToConstraints (SymListTy symTypeN) freshType
        let subConstraintsP = reduceSubtypesToConstraints symTypeP freshType

        let constraints = constraintsN @ constraintsP @ weightConstraints @ subConstraintsN @ subConstraintsP

        (newLocType, constraints)

    | FoldList(N, P, x, y, Q) ->
        let locTypeN, constraintsN = generateConstraintRec context N bounds gen
        let (symTypeN, weightN) = locTypeN.[LocEpsilon]

        let locTypeP, constraintsP = generateConstraintRec context P bounds gen
        let (symTypeP, weightP) = locTypeP.[LocEpsilon]

        // Inspect the type of N, to determine the type of x and y in the context for Q
        match symTypeN with
            | SymListTy innerType ->
                let context' =
                    context
                    |> Map.add x innerType
                    |> Map.add y (SymListTy innerType)

                let locTypeQ, constraintsQ = generateConstraintRec context' Q bounds gen
                let (symTypeQ, weightQ) = locTypeQ.[LocEpsilon]

                let totalWeight, weightConstraints =
                    let freshBranchWeight = gen()
                    let weightConstraints1 = [ Subset(weightP, freshBranchWeight); Subset(weightQ, freshBranchWeight) ]
                    let tW, weightConstraints2 = multiplyWeights gen [freshBranchWeight; weightN]
                    tW, weightConstraints1 @ weightConstraints2

                // Generate a new type for the returned type (symTypeQ would also work)
                let freshReturnType = generateSymbolicType gen (baseType symTypeP)

                // Ensure that both branches match the return type
                let subtypeConstraints =
                    reduceSubtypesToConstraints symTypeP freshReturnType
                    @ reduceSubtypesToConstraints symTypeQ freshReturnType

                let cT = (freshReturnType, totalWeight)

                let newLocType =
                    Util.modifyMapKey [(LocFoldListMatch, locTypeN); (LocFoldListEmpty, locTypeP); (LocFoldListCons, locTypeQ)]
                    |> Map.add LocEpsilon cT

                let constraints = constraintsN @ constraintsP @ constraintsQ @ subtypeConstraints @ weightConstraints

                (newLocType, constraints)

            | _ -> failwith "Not well typed"
    | PrimF (functionSymbol, args)
    | SymPrimF (functionSymbol, args) ->
        let locactionWrapper = match M with PrimF _ -> LocPrimF | _ -> LocSymPrimF

        // Infer types of all arguments
        let typedArgs =
            List.map (fun arg -> generateConstraintRec context arg bounds gen) args

        let argConstraints = List.collect snd typedArgs

        let argVars =
            typedArgs
            |> List.map
                (fun (m , _) ->
                    match m.[LocEpsilon] with
                    | SymReal i, _ -> i
                    | _ -> failwith "Not well typed")
        let argWeights =
            typedArgs
            |> List.map
                (fun (m , _) ->
                    let _, w = m.[LocEpsilon]
                    w)

        let result = gen ()
        let (weight, weightConstraints) = multiplyWeights gen argWeights

        let cT = (SymReal result, weight)

        let newLocType =
            Util.modifyMapKey (
                List.mapi
                    (fun i (m, _) -> ((fun y -> locactionWrapper(i, y)), m) )
                    typedArgs
            )
            |> Map.add LocEpsilon cT

        // Ensure that result is the application of the function to argVars
        (newLocType, FunctionResult(result, functionSymbol, argVars) :: (weightConstraints @ argConstraints))
    | Tuple args  ->
        let typedArgs =
            List.map (fun arg -> generateConstraintRec context arg bounds gen) args
        let argConstraints = List.collect snd typedArgs
        let argTypes =
            typedArgs
            |> List.map (fun (m , _) -> fst m.[LocEpsilon])
        let argWeights =
            typedArgs
            |> List.map (fun (m , _) -> snd m.[LocEpsilon])

        let (weight, weightConstraints) = multiplyWeights gen argWeights
        let cT = (SymTupleTy argTypes, weight)
        let newLocType =
            Util.modifyMapKey (
                List.mapi
                    (fun i (m, _) -> ((fun y -> LocTuple(i, y)), m) )
                    typedArgs
            )
            |> Map.add LocEpsilon cT

        (newLocType, weightConstraints @ argConstraints)

    | TupleMatch(vars, N, P) ->
        let locTypeN, constraintsN = generateConstraintRec context N bounds gen
        let (symTypeN, weightN) = locTypeN.[LocEpsilon]

        match symTypeN with
            | SymTupleTy typeList ->
                // Add the type of each component of the tuple to the typing context of P
                let context' =
                    List.zip vars typeList
                    |> List.fold (fun s (x, t) -> Map.add x t s) context

                let locTypeP, constraintsP = generateConstraintRec context' P bounds gen
                let (symTypeP, weightP) = locTypeP.[LocEpsilon]

                let w, weightConstraints = multiplyWeights gen [weightN; weightP]

                let cT = symTypeP, w
                let newLocType =
                    Util.modifyMapKey [(LocTupleMatchArg, locTypeN); (LocTupleMatchBody, locTypeP)]
                    |> Map.add LocEpsilon cT

                (newLocType, constraintsN @ constraintsP @ weightConstraints)

            | _ -> failwith "Not well typed"


/// Generates the Constraints of an expression starting from the empty environment
let generateConstraint (M: Expr) (bounds: VarBoundMap) gen =
    generateConstraintRec Map.empty M bounds gen
