(****************************************************************************************)
(*                                                                                      *)
(*                                      Util.fs                                         *)
(*                                                                                      *)
(****************************************************************************************)
(*                                                                                      *)
(* Various helping constructs                                                           *)
(*                                                                                      *)
(****************************************************************************************)

module Util


open System
open Interval

type Box = Interval[]
type VarBoundMap = Map<int, Interval>

type Compare =
    | LEQ
    | LT
    | GT
    | GEQ

/// Compute the cartesian product
let rec cart1 (LL: list<list<'a>>) =
    match LL with
    | [] -> Seq.singleton []
    | L :: Ls ->
        seq {
            for x in L do
                for xs in cart1 Ls -> x :: xs
        }

/// Join a sequence of maps
let joinMaps (mapSeq: seq<Map<'a, 'b>>) =
    let combination =
        mapSeq
        |> Seq.collect Map.toSeq
        |> Seq.groupBy fst
        |> Seq.map (fun (x, y) -> x, (Seq.map snd y) |> set)

    if Seq.forall (fun (_, x) -> Set.count x = 1) combination then
        // Is a valid Map
        combination
        |> Seq.map (fun (x, y) -> x, Set.minElement y)
        |> Map.ofSeq
        |> Some
    else
        None

/// Merges a list of maps using joinMaps
let mergeLLMap (a: list<list<Map<'a, 'b>>>) =
    let res = a |> cart1 |> Seq.choose joinMaps
    res


// A type representing the Discretisation computed by GuBPI, i.e, the size and position of the buckets.
type DomainDiscretisation =
    { Start: double
      End: double
      StepSize: double }

    member this.NumberOfBins =
        (this.End - this.Start) / this.StepSize
        |> Math.Ceiling
        |> int

type ApproximationMethod =
    | Polytopes
    | Boxes

// The type for the GuBPI Hyperparameters
type Hyperparameters =
    {
      // Which method to use. If Polytopes is selected but a guard is non linear, we resort to Boxes. Boxes guartees that the box method is used
      method: ApproximationMethod

      // How to discrete the denotation
      discretization: DomainDiscretisation

      // The depth of the symbolic exploration
      depth: int

      splits: int

      // Determines how fine score values should be split (in the linear optimization)
      epsilonScore: double

      // Determines how fine score sample variables should be split (in the linear optimization)
      epsilonVar: double

      // Determines the intermediate outputs of GuBPI during evaluation
      outputSplitProgress: bool
      outputCurrentPath: bool
      outputCurrentArea: bool}


/// Combines two maps by applying a function f on each shared key
let combineMapping (A: Map<int, double>) (B: Map<int, double>) (f: double * double -> double) =
    let mutable newMap = Map.empty
    // Iterate over every pair
    for k in A |> Map.toSeq |> Seq.map fst do
        // Find the coenficnet in A
        let wa = Map.find k A
        // Find the coenficnet in B. If it does not exists use 0.0
        let wb =
            Map.tryFind k B |> Option.defaultValue 0.0
        // Add combined Value to newMap
        newMap <- Map.add k (f (wa, wb)) newMap

    for k in B |> Map.toSeq |> Seq.map fst do
        // Find the coenficnet in A
        let wb = Map.find k B
        // Find the coenficnet in B. If it does not exists use 0.0
        let wa =
            Map.tryFind k A |> Option.defaultValue 0.0
        // Add combined Value to newMap
        newMap <- Map.add k (f (wa, wb)) newMap

    newMap

let addMaps (A: Map<'a, 'b>) (B: Map<'a, 'b>) =
    let t = Seq.append (Map.toSeq A) (Map.toSeq B)
    Map.ofSeq t

let addMapList (A: seq<Map<'a, 'b>>) = A |> Seq.collect Map.toSeq |> Map.ofSeq

let modifyMapKey l =
    l
    |> Seq.collect (fun (f, x) -> x |> Map.toSeq |> Seq.map (fun (x, y) -> (f x, y)))
    |> Map.ofSeq



module SubprocessUtil =
    type SubprocessResult =
        | SubprocessOutcome of String * int
        | SubprocessError of String

    type private ProcessResult =
        { ExitCode: int;
        Stdout: string;
        Stderr: string }

    let private executeProcess (exe, cmdline) =
        let psi =
            System.Diagnostics.ProcessStartInfo(exe, cmdline)

        psi.UseShellExecute <- false
        psi.RedirectStandardOutput <- true
        psi.RedirectStandardError <- true
        psi.CreateNoWindow <- true
        let p = System.Diagnostics.Process.Start(psi)
        let output = System.Text.StringBuilder()
        let error = System.Text.StringBuilder()
        p.OutputDataReceived.Add(fun args -> output.Append(args.Data) |> ignore)
        p.ErrorDataReceived.Add(fun args -> error.Append(args.Data) |> ignore)
        p.BeginErrorReadLine()
        p.BeginOutputReadLine()
        p.WaitForExit()

        { ExitCode = p.ExitCode;
        Stdout = output.ToString();
        Stderr = error.ToString() }

    let exec (cmd: string) (arg: string) = 
        let res= executeProcess(cmd, arg)

        if res.Stderr <> "" then   
            SubprocessError res.Stderr
        else 
            SubprocessOutcome (res.Stdout, res.ExitCode)