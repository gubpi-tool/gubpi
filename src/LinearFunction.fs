(****************************************************************************************)
(*                                                                                      *)
(*                                      LinearFunction.fs                               *)
(*                                                                                      *)
(****************************************************************************************)
(*                                                                                      *)
(* Operations on linear functions and interval linea functions.                         *)
(*                                                                                      *)
(****************************************************************************************)

module LinearFunction

open System

open Interval
open Util

type LinearFunction =
    { Coefficients: Map<int, double>
      Offset: double }

    static member FromConstant(c: double) =
        { Coefficients = Map.empty; Offset = c }

    static member FromVar(var: int) =
        let coe = [ (var, 0.0) ] |> Map.ofList

        { Coefficients = coe; Offset = 0.0 }

    static member (~-)(lf: LinearFunction) =
        { Coefficients = Map.map (fun _ c -> -c) lf.Coefficients
          Offset = -lf.Offset }

    static member (+)(lf1: LinearFunction, lf2: LinearFunction) =
        let m =
            Util.combineMapping lf1.Coefficients lf2.Coefficients (fun (x, y) -> x + y)

        { Coefficients = m
          Offset = lf1.Offset + lf2.Offset }

    static member (-)(lf1: LinearFunction, lf2: LinearFunction) =
        let m =
            Util.combineMapping lf1.Coefficients lf2.Coefficients (fun (x, y) -> x - y)

        { Coefficients = m
          Offset = lf1.Offset - lf2.Offset }

    member this.UsedVars =
        this.Coefficients
        |> Map.toSeq
        |> Seq.map fst
        |> set

    static member (*)(this: LinearFunction, factor: double) =
        { Coefficients = Map.map (fun _ x -> x * factor) this.Coefficients
          Offset = this.Offset * factor }

    member this.IsFinite = Double.IsFinite this.Offset

    member this.EvalWithGivenBounds(m: VarBoundMap) =
        let mutable b = precisely this.Offset

        for (key, c) in Map.toSeq this.Coefficients do
            b <- b + (point c) * m.[key]

        b

    member this.EvalBox(box: Box) =
        let mutable b = precisely this.Offset

        for (key, c) in Map.toSeq this.Coefficients do
            b <- b + (point c) * box.[key]

        b

    member this.PluginInterval(m: VarBoundMap) =
        let mutable b = precisely this.Offset

        let mutable newCoefs = Map.empty

        for (key, c) in Map.toSeq this.Coefficients do
            if Map.containsKey key m then
                b <- b + m.[key]
            else
                newCoefs <- Map.add key c newCoefs

        if Map.isEmpty newCoefs then
            None, b
        else
            (Some
                { Coefficients = newCoefs
                  Offset = 0.0 }),
            b

/// A linear inequality has the form "f \bowtie t" for a linear function f, constant t and comparison operator \boxtie
type LinearInequality =
    { Function: LinearFunction
      Com: Compare
      Threshold: double }

    member this.UsedVars = this.Function.UsedVars

    member this.isFinite = this.Function.IsFinite


/// Similar to a linear Function but with an addition interval added to the function (for example "3 x + [-1, 9] + 5")
type IntervalLinearFunction =
    { Linear: LinearFunction
      AddedInterval: Interval }

    member this.Coefficients = this.Linear.Coefficients
    member this.Offset = this.Linear.Offset

    static member (+)(lf1: IntervalLinearFunction, lf2: IntervalLinearFunction) =
        { Linear = lf1.Linear + lf2.Linear
          AddedInterval = lf1.AddedInterval + lf2.AddedInterval }

    static member (-)(lf1: IntervalLinearFunction, lf2: IntervalLinearFunction) =
        { Linear = lf1.Linear - lf2.Linear
          AddedInterval = lf1.AddedInterval - lf2.AddedInterval }

    static member (~-)(lf: IntervalLinearFunction) =
        { IntervalLinearFunction.Linear = -lf.Linear
          AddedInterval = -lf.AddedInterval }

    static member (*)(lf: IntervalLinearFunction, c: double) =
        { Linear = lf.Linear * c
          AddedInterval = lf.AddedInterval * precisely c }

    member this.UsedVars = this.Linear.UsedVars

type IntervalLinearInequality =
    { Function: IntervalLinearFunction
      Com: Compare
      Threshold: double }

    member this.UsedVars = this.Function.UsedVars
