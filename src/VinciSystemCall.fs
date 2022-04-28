(****************************************************************************************)
(*                                                                                      *)
(*                                      VinciSystemCall.fs                              *)
(*                                                                                      *)
(****************************************************************************************)
(*                                                                                      *)
(* Handles the System Calls to Vinci                                                    *)
(*                                                                                      *)
(****************************************************************************************)
module VinciSystemCall

type ProcessResult =
    { ExitCode: int
      Stdout: string
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

    { ExitCode = p.ExitCode
      Stdout = output.ToString()
      Stderr = error.ToString() }

let exec (cmd: string) (arg: string) : string = executeProcess(cmd, arg).Stdout

let vinciStopwatch = System.Diagnostics.Stopwatch()

// Computes the volume by calling the external tool vinci and parsing its output
let computeVolumeVinci (s: string) : double =
    try
        vinciStopwatch.Start()
        let out = exec "./vinci" ("\"" + s + "\"")
        vinciStopwatch.Stop()

        if out.Contains "unbounded!" then
            System.Double.PositiveInfinity
        else
            double (out)
    with
    | ex ->
        printfn "An error occured while performing analysis via vinci."

        printfn $"The input was %s{s}\n"

        printfn $"The error was\n%A{ex}"

        raise ex
        exit 0
