(****************************************************************************************)
(*                                                                                      *)
(*                                      VinciSystemCall.fs                              *)
(*                                                                                      *)
(****************************************************************************************)
(*                                                                                      *)
(* Handles the System Calls to Vinci                                                    *)
(*                                                                                      *)
(****************************************************************************************)
module SystemCall
#nowarn "59"

open System

type SystemCallResult = 
    | SystemCallOutcome of String
    | SystemCallError of String
    | SystemCallTimeout

let systemCall cmd arg timeout =
    let p = new System.Diagnostics.Process();
    p.StartInfo.RedirectStandardOutput <- true
    p.StartInfo.RedirectStandardError <- true
    p.StartInfo.UseShellExecute <- false
    p.StartInfo.FileName <- cmd
    p.StartInfo.Arguments <- arg
    p.Start() |> ignore 

    let a = 
        match timeout with 
            | Option.None -> 
                true
            | Some t -> 
                p.WaitForExit(t :> int)

    if a then 
        let err = p.StandardError.ReadToEnd() 

        if err <> "" then 
            SystemCallError err
        else 
            let res = p.StandardOutput.ReadToEnd()
            p.Kill true
            SystemCallOutcome res
    else 
        p.Kill true
        SystemCallTimeout

let vinciStopwatch = System.Diagnostics.Stopwatch()
let lpSolveStopwatch = System.Diagnostics.Stopwatch()

// Computes the volume by calling the external tool vinci and parsing its output
let computeVolumeVinci (s: string) : double =
    // We get the path of the GuBPI executable. By convention, the vinci execuatble is located in the same dirctory
    let vinciPath = 
        System.IO.Path.Join [|System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location); "vinci"|]

    vinciStopwatch.Start()
    let out = systemCall vinciPath ("\"" + s + "\"") None
    vinciStopwatch.Stop()

    match out with 
    | SystemCallOutcome out -> 
        if out.Contains "unbounded!" then
            System.Double.PositiveInfinity
        else
            double (out)

    | SystemCallTimeout ->
        printfn "Vinci timed out"
        exit 0
            
    | SystemCallError err -> 
        printfn "An error occured while performing analysis via vinci."
        printfn $"The input was %s{s}\n"
        printfn $"The error was:\n%A{err}"
        exit 0
