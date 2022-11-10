(****************************************************************************************)
(*                                                                                      *)
(*                                      Program.fs                                      *)
(*                                                                                      *)
(****************************************************************************************)
(*                                                                                      *)
(* The entry point for GuBPI                                                            *)
(*                                                                                      *)
(****************************************************************************************)

open System
open System.IO

open Interval
open Util
open Parser
open Typing
open Expression
open Evaluation

// The default configuration that is used if no hyper parameters are changed
let defaultConfig =
    { method = Polytopes
      discretization =
        { Start = -4.0
          End = 4.0
          StepSize = 0.25 }
      depth = 20
      splits = 10000
      epsilonScore = 0.025
      epsilonVar = 0.05
      outputSplitProgress = true
      outputCurrentPath = true
      outputCurrentArea = true }


[<EntryPoint>]
let main argv =

    try

        let filePath =
            if argv.Length = 0 then
                printfn "No input file specified. Provide the file name as a command-line argument"
                exit 0
            else
                argv.[0]

        let fileName =
            Path.GetFileNameWithoutExtension(filePath)

        let inputProgram = File.ReadAllText filePath

        // Parse the Term and the Configuration containing the parameters for GuBPI
        let config, term =
            try
                parseProgram defaultConfig inputProgram
            with
                | _ ->
                    // Error Message is already displayed in parseProgram
                    exit 0


        GlobalConstants.outputSplitProgress <- config.outputSplitProgress
        GlobalConstants.outputCurrentPath <- config.outputCurrentPath
        GlobalConstants.outputCurrentArea <- config.outputCurrentArea

        printfn $"{config}"

        let dd = config.discretization

        // Check if the term is typeable. If so, we add type annotations to the term
        let term =
            match Typing.checkAndAnnotateType term with
            | None ->
                printfn "The provided term cannot be typed."
                exit 0
            | Some M -> M

        // Translate the SPCF term to anglican
        let anglicanFilePath = $"output/{fileName}.clj"
        (new FileInfo(anglicanFilePath)).Directory.Create() // Make sure that the output folder is created. If its already exists, this method does nothing
        File.WriteAllText(anglicanFilePath, translateToAnglican term fileName)

        let stopwatch = Diagnostics.Stopwatch()
        stopwatch.Start()

        // Compute all symbolic paths up to depth config.depth.
        // evalQueue already performs the sound removal of fixpoints using the interval-type system
        let programPaths =
            try
                evalQueue config.depth term
            with
                | _ ->
                    printfn "An error occured during symbolic anaylsis."
                    if not GlobalConstants.catchExceptions then
                        reraise()
                    exit 0

        printfn $"Found {programPaths.Length} paths."

        let bounds, outerBounds =
            if GlobalConstants.runParallel then
                // Run the evaluation in parallel. This usually performs worse than running it sequentially and might induce additional errors.
                let asynchWorker p =
                    async {
                        let pathBounds, pathOuterBounds = Analysis.computeHistogram p config

                        return pathBounds, pathOuterBounds
                    }

                let results =
                    programPaths
                    |> List.map asynchWorker
                    |> fun x -> Async.Parallel(x, 2)
                    |> Async.RunSynchronously

                let bounds =
                    Array.fold (fun s (x, _) -> Array.map2 (+) x s) (Array.create dd.NumberOfBins Interval.Zero) results

                let outerBounds =
                    Array.fold (fun s (_, x) -> x + s) Interval.Zero results

                bounds, outerBounds
            else
                // Perform the analysis sequentially
                let mutable outerBounds = Interval.Zero

                let mutable bounds =
                    Array.create dd.NumberOfBins Interval.Zero

                let mutable currentCount = 0

                for p in programPaths do
                    if GlobalConstants.outputCurrentPath then
                        printfn "Working on path %i/%i" currentCount programPaths.Length
                        currentCount <- currentCount + 1

                    // For each symbolic path we compute the bounds given by config.discretization
                    // pathOuterBounds contains the bounds for the are not covered by config.discretization
                    let pathBounds, pathOuterBounds =
                        try
                            Analysis.computeHistogram p config
                        with
                            | _ ->
                                printfn "An error occured during the anaylsis of a symbolic path. The path likely contains a combination that is currently not supported by GuBPI"
                                if not GlobalConstants.catchExceptions then
                                    reraise()
                                exit 0

                    // Add the bounds computed on p to the overall bounds
                    bounds <- Array.map2 (+) bounds pathBounds
                    outerBounds <- outerBounds + pathOuterBounds

                bounds, outerBounds

        stopwatch.Stop()



        // Analysis is finished, the bounds are computed and contained in bounds and outerBounds. We now output and plot the bounds.

        // Compute the middle point for each of the bins of the bar plot.
        let xPoints =
            [| for i in 0 .. bounds.Length - 1 ->
                let x = dd.Start + dd.StepSize * double i
                let x' = min(dd.Start + dd.StepSize * double (i + 1)) dd.End
                x + ((x' - x) / 2.0)
            |]

        // We plot unbounded bounds by mapping them to -1 (for visualization)
        let removeInfty =
            Array.map (fun r -> if Double.IsFinite r then r else -1.0)

        let scale =
            Array.map (fun (x: double) -> x / dd.StepSize)

        // Plot the unnormalized bounds to file {fileName}-unnorm.html
        Plotting.plotTwo
            Plotting.DenType.UNNORM
            xPoints
            (Array.map lo bounds |> scale |> removeInfty)
            (Array.map hi bounds |> scale |> removeInfty)
            $"output/{fileName}-unnorm.html"

        // Write the unnormalized bounds to file {fileName}-unnorm.bounds
        using (File.CreateText($"output/{fileName}-unnorm.bounds")) (fun file ->
            for i = 0 to bounds.Length - 1 do
                let x = dd.Start + dd.StepSize * double i
                let x' = min(dd.Start + dd.StepSize * double (i + 1)) dd.End
                let b = bounds.[i]
                fprintfn file $"[{x:G9}, {x':G9}]: [{b.lo:G9}, {b.hi:G9}]"

            fprintfn file $"outside: [{outerBounds.lo:G9}, {outerBounds.hi:G9}]")

        // Compute interval approximation of the normalizing constant and compute the normalized Bounds

        let programContainsScore = term.ContainsScore

        // Compute the estimate infered from the bounds
        let normConstantEstimate = Array.sum bounds + outerBounds

        // If the program contains no score, the normConst is 1 (We assume the program to be AST)
        let normConstant = if programContainsScore then normConstantEstimate else Interval.One

        // Compute the normalized bounds
        let normalizedBounds =
            Array.map (fun i -> i / normConstant) bounds
        let normalizedOuterBounds = outerBounds / normConstant

        // Print the normalized bounds to {fileName}-norm.bounds
        using (File.CreateText($"output/{fileName}-norm.bounds")) (fun file ->
            for i = 0 to bounds.Length - 1 do
                let x = dd.Start + dd.StepSize * double i
                let x' = min(dd.Start + dd.StepSize * double (i + 1)) dd.End
                let b = normalizedBounds.[i]
                fprintfn file $"[{x:G9}, {x':G9}]: [{b.lo:G9}, {b.hi:G9}]"

            fprintfn file $"outside: [{normalizedOuterBounds.lo:G9}, {normalizedOuterBounds.hi:G9}]")

        // Plot the normalized bounds to {fileName}-norm.html
        Plotting.plotTwo
            Plotting.DenType.NORM
            xPoints
            (Array.map lo normalizedBounds
            |> scale
            |> removeInfty)
            (Array.map hi normalizedBounds
            |> scale
            |> removeInfty)
            $"output/{fileName}-norm.html"

        if programContainsScore then
            printfn $"Normalizing constant: {normConstant.lo} <= Z <= {normConstant.hi}"
        else
            printfn $"The program contains no score, so the normalizing constant is 1. The computed bounds inferred give: {normConstantEstimate.lo} <= Z <= {normConstantEstimate.hi}"

        printfn $"Total computation time: %.3f{double stopwatch.ElapsedMilliseconds / 1000.0} seconds"

        printfn
            "Vinci computation: %.3f seconds"
            (double VinciSystemCall.vinciStopwatch.ElapsedMilliseconds
            / 1000.0)

        printfn
            "Flips computation: %.3f seconds"
            (double LinearOptimization.linearOptStopwatch.ElapsedMilliseconds
            / 1000.0)

    with
        | _ ->
            printfn "An unexpected error occured."
            if not GlobalConstants.catchExceptions then
                reraise()
            exit 1

    0
