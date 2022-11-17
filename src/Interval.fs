(****************************************************************************************)
(*                                                                                      *)
(*                                      Interval.fs                                     *)
(*                                                                                      *)
(****************************************************************************************)
(*                                                                                      *)
(* The definition of intervals and interval arithmetic                                  *)
(*                                                                                      *)
(****************************************************************************************)

module Interval

open System

/// Gives the next larger double value
let nextLarger (x: double) =
    if Double.IsFinite x then
        let bits = BitConverter.DoubleToInt64Bits x

        let larger =
            if bits >= 0L then bits + 1L
            elif bits = Int64.MinValue then 1L // x = -0.0
            else bits - 1L

        BitConverter.Int64BitsToDouble larger
    else
        x
        
/// Gives the next smaller double value
let nextSmaller (x: double) =
    if Double.IsFinite x then
        let bits = BitConverter.DoubleToInt64Bits x

        let smaller =
            if bits >= 1L then bits - 1L
            elif bits = 0L then Int64.MinValue + 1L // x = +0.0
            else bits + 1L

        BitConverter.Int64BitsToDouble smaller
    else
        x

[<Struct>]
type Interval =
    val lo: double
    val hi: double

    new(lo: double, hi: double, inflate: bool) =
        { lo = if inflate then nextSmaller lo else lo
          hi = if inflate then nextLarger hi else hi }

    new(x: double) = Interval(x, x, true)
    new(lo: double, hi: double) = Interval(lo, hi, true)

    member inline this.CheckNaN() : Interval =
        if Double.IsNaN(this.lo) || Double.IsNaN(this.hi) then
            Interval(-infinity, infinity, false)
        else
            this

    member inline this.ToPair: double * double = this.lo, this.hi
    member inline this.Contains(x: double) : bool = this.lo <= x && x <= this.hi
    member this.Diam() : double = this.hi - this.lo
    member this.Center() : double = this.lo + 0.5 * this.Diam()
    override this.ToString() : string = $"[{this.lo}, {this.hi}]"

    static member Zero = Interval(0.0, 0.0, false)
    static member One = Interval(1.0, 1.0, false)

    static member inline (+)(x: Interval, y: Interval) : Interval =
        // These special cases were not an issue at the time of writing,
        // but a similar case analysis was necessary for multiplication, so it seems like a good idea.
        let lo =
            match x.lo, y.lo with
            | 0.0, _ -> y.lo
            | _, 0.0 -> x.lo
            | _, _ -> nextSmaller (x.lo + y.lo)

        let hi =
            match x.hi, y.hi with
            | 0.0, _ -> y.hi
            | _, 0.0 -> x.hi
            | _, _ -> nextLarger (x.hi + y.hi)

        Interval(lo, hi, false)

    static member inline (~-)(x: Interval) : Interval = Interval(-x.hi, -x.lo, false)
    static member inline (-)(x: Interval, y: Interval) : Interval = x + (-y)

    static member inline (*)(x: Interval, y: Interval) : Interval =
        // We have to handle the numbers 0.0 and 1.0 special
        // because we don't want to do too much rounding.
        // That would lead to issues in combination with widening: intervals could be widened to [-infty, infty].

        let a = x.lo * y.lo
        let b = x.lo * y.hi
        let c = x.hi * y.lo
        let d = x.hi * y.hi

        let isExact (z: Double) = z = 0.0 || z = 1.0 || z = -1.0 // multiplication with these numbers is exact

        let xLoExact = isExact x.lo
        let xHiExact = isExact x.hi
        let yLoExact = isExact y.lo
        let yHiExact = isExact y.hi

        let aExact = xLoExact || yLoExact
        let bExact = xLoExact || yHiExact
        let cExact = xHiExact || yLoExact
        let dExact = xHiExact || yHiExact

        let nextSmallerUnless (cond: bool) (z: double) = if cond then z else nextSmaller z

        let aDown = nextSmallerUnless aExact a
        let bDown = nextSmallerUnless bExact b
        let cDown = nextSmallerUnless cExact c
        let dDown = nextSmallerUnless dExact d

        let nextLargerUnless (cond: bool) (z: double) = if cond then z else nextLarger z

        let aUp = nextLargerUnless aExact a
        let bUp = nextLargerUnless bExact b
        let cUp = nextLargerUnless cExact c
        let dUp = nextLargerUnless dExact d

        let lo = min (min aDown bDown) (min cDown dDown)
        let hi = max (max aUp bUp) (max cUp dUp)
        let result = Interval(lo, hi, false).CheckNaN()

        if x.lo >= 0.0 && y.lo >= 0.0 then
            Interval(max 0.0 result.lo, result.hi, false)
        else
            result // otherwise ε * ε rounds down to -ε

    member inline this.Inverse() : Interval =
        if this.lo >= 0.0 || this.hi <= 0.0 then
            let hi =
                if this.lo = 1.0 || this.lo = -1.0 then
                    this.lo
                else
                    nextLarger (1.0 / this.lo)

            let lo =
                if this.hi = 1.0 || this.hi = -1.0 then
                    this.hi
                else
                    nextSmaller (1.0 / this.hi)

            Interval(lo, hi, false)
        else
            Interval(-infinity, infinity)

    static member inline (/)(x: Interval, y: Interval) : Interval = x * y.Inverse()

    member inline this.Exp() : Interval =
        Interval(Math.Exp(this.lo), Math.Exp(this.hi))

    member inline this.Log() : Interval =
        Interval(Math.Log(max this.lo 0.0), Math.Log(max this.hi 0.0))

    member inline this.Squared() : Interval =
        let lo2 = this.lo * this.lo
        let hi2 = this.hi * this.hi

        if this.Contains(0.0) then
            Interval(0.0, max lo2 hi2)
        else
            Interval(min lo2 hi2, max lo2 hi2)

    member inline this.Sqrt() : Interval =
        Interval(Math.Sqrt(max this.lo 0.0), Math.Sqrt(max this.hi 0.0))

    member inline this.Overlaps(other: Interval) : bool =
        this.lo <= other.hi && other.lo <= this.hi

    member inline this.StrictlyOverlaps(other: Interval) : bool =
        this.lo < other.hi && other.lo < this.hi

    member inline this.Abs() : Interval =
        let lo = if this.lo <= 0.0 && 0.0 <= this.hi then 0.0 else min (abs this.lo) (abs this.hi)
        let hi = max (abs this.lo) (abs this.hi)
        Interval(lo, hi)

let allReals = Interval(-infinity, infinity)
let unitInterval = Interval(0.0, 1.0, false)

let preciseInterval (lo: double) (hi: double) = Interval(lo, hi, false)

let itv (lo: double) (hi: double) = Interval(lo, hi, true)

let point (x: double) = Interval(x, x, true)

let precisely (x: double) = Interval(x, x, false)

let zeroTo (x: double) = Interval(0.0, nextLarger x, false)

let lo (iv: Interval) = iv.lo

let hi (iv: Interval) = iv.hi

let diam (iv: Interval) = iv.Diam()

let hull (iv1: Interval) (iv2: Interval) : Interval =
    preciseInterval (min iv1.lo iv2.lo) (max iv1.hi iv2.hi)

let intersect (iv1: Interval) (iv2: Interval) : Interval =
    let lo = max iv1.lo iv2.lo
    let hi = min iv1.hi iv2.hi

    if lo > hi then
        failwith $"Intersection of {iv1} and {iv2} is empty."

    preciseInterval lo hi

let ensureNonnegative (iv: Interval) =
    if iv.lo < 0.0 then
        Interval(0.0, iv.hi, false)
    else
        iv

let pointNonneg (x: double) = point x |> ensureNonnegative
