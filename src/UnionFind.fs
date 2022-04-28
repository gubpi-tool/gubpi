(****************************************************************************************)
(*                                                                                      *)
(*                                      UnionFind.fs                                    *)
(*                                                                                      *)
(****************************************************************************************)
(*                                                                                      *)
(* Simple Union Find Structure without path compression                                 *)
(*                                                                                      *)
(****************************************************************************************)

module UnionFind

/// A simple Union Find Structure for n elements without path compression
type UnionFind(n: int) =
    
    // parents maps each node to its parent
    let mutable parents =
        [| for i in 0 .. (n - 1) do
               i |]

    /// Gives the root of the tree containing i
    let rec find (i: int) =
        if (parents.[i] = i) then
            i
        else
            find (parents.[i])

    /// Joins i and j
    member this.Union(i, j) =
        let fi = find i
        let fj = find j

        if (fi = fj) then
            ()
        else
            parents.[fi] <- fj

    /// Joins all the elements in s
    member this.UnionMany(s: Set<int>) =
        if Set.isEmpty s then
            ()
        else
            let l: list<int> = Set.toList s
            let s = List.head l
            List.iter (fun x -> this.Union(x, s)) (List.tail l)

    /// Outputs the current partition as an array of sets
    member this.ExtractPartition() =
        // First, map all roots to a fresh number
        let mutable m = Map.empty
        let mutable count = 0

        for i in 0 .. (n - 1) do
            if i = parents.[i] then
                //i is the root of a tree, add to m
                m <- m.Add(i, count)
                count <- count + 1
            else
                ()

        let mutable out = Array.create m.Count Set.empty

        for i in 0 .. (n - 1) do
            // Find the root of i
            let pos = Map.find (find i) m
            out.[pos] <- out.[pos].Add(i)

        out
