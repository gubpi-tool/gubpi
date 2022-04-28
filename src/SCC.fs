(****************************************************************************************)
(*                                                                                      *)
(*                                      SCC.fs                                          *)
(*                                                                                      *)
(****************************************************************************************)
(*                                                                                      *)
(* helper Function to compute the SCCs in a directed graph                              *)
(*                                                                                      *)
(****************************************************************************************)

module SCC

open System.Collections.Generic

/// SCC algorithm for nodes 0..maxNode. 
let computeSCC (maxNode: int) (forwardEdges: array<Set<int>>) =
    let backwardEgdes = Array.create (maxNode + 1) Set.empty

    let visited = Array.create (maxNode + 1) false

    let mutable l = []

    let rec visit n =
        if not visited.[n] then
            Array.set visited n true

            for n' in forwardEdges.[n] do
                visit n'
                // Populate the backward edges
                Array.set backwardEgdes n' (Set.add n backwardEgdes.[n'])

            l <- n :: l

    for n in 0 .. maxNode do
        visit n

    let wasAssigned = Array.create (maxNode + 1) false

    let rec assign n : Set<int> =
        if wasAssigned.[n] |> not then
            Array.set wasAssigned n true

            let temp =
                [ for n' in backwardEgdes.[n] do
                      assign n' ]
                |> Set.unionMany

            Set.add n temp
        else
            Set.empty

    let mutable components = []

    for n in l do
        if wasAssigned.[n] |> not then
            components <- assign n :: components

    // Reverse the list
    components |> List.rev

/// Computes a topological Order of a DAG given by its edges
let computeTopoOrder (forwardEdges: Map<int, Set<int>>) =

    let allNodes =
        forwardEdges
        |> Map.toSeq
        |> Seq.map fst

    let mutable visited = Set.empty

    let stack = new Stack<int>()

    let rec visit n =
        visited <- Set.add n visited

        for n' in forwardEdges.[n] do
            if not (Set.contains n' visited) then visit n'

        stack.Push n

    // Vist all nodes
    for n in allNodes do
        if not (Set.contains n visited) then visit n

    stack.ToArray() |> Array.toList
