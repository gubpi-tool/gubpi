(****************************************************************************************)
(*                                                                                      *)
(*                                       Weight.fs                                      *)
(*                                                                                      *)
(****************************************************************************************)
(*                                                                                      *)
(* A nonnegative floating point type using logarithmic representation.                  *)
(*                                                                                      *)
(****************************************************************************************)

module Weight

open System
open Interval

/// A nonnegative number represented by its natural logarithm.
///
/// This is useful for weights, likelihoods and probabilities, as it prevents underflow.
[<Struct>]
type Weight =
    val log: double

    new(log: double) = { log = log }

    static member Zero = Weight(-infinity)
    static member One = Weight(0.0)
    static member Infinity = Weight(infinity)

    static member inline From(x: double) : Weight = Weight(log x)
    member inline this.isZero() : bool = Double.IsNegativeInfinity this.log
    member inline this.isOne() : bool = this.log = 0.0

    static member inline (+)(x: Weight, y: Weight) : Weight =
        if x.isZero () then
            y
        elif y.isZero () then
            x
        else
            let (max, min) =
                if x.log > y.log then
                    (x.log, y.log)
                else
                    (y.log, x.log)
            // for better precision, we should use log1p(exp(min - max)) if F# ever supports that
            Weight(max + log (exp (min - max) + 1.0))

    static member inline (-)(x: Weight, y: Weight) : Weight =
        if y.isZero () then
            x
        else
            // for better precision, we should use log1p(-exp(y.log - x.log)) if F# ever supports that
            Weight(x.log + log (1.0 - exp (y.log - x.log)))

    static member inline (*)(x: Weight, y: Weight) : Weight = Weight(x.log + y.log)

    member inline this.Inverse() = Weight(-this.log)

    static member inline (/)(x: Weight, y: Weight) : Weight = Weight(x.log - y.log)

    member inline this.toDouble: float = exp this.log
    member inline this.isNaN: bool = Double.IsNaN(this.log)

    member inline this.isFinite: bool =
        Double.IsFinite this.log
        || Double.IsNegativeInfinity this.log

    override this.ToString() : string = $"{this.toDouble}"

let toDouble (w: Weight) : float = exp w.log
let toWeight (x: float) : Weight = Weight.From(x)

/// Gives the next larger Weight value
let nextLarger (w: Weight) : Weight = Weight(nextLarger w.log)

/// Gives the next smaller Weight value
let nextSmaller (w: Weight) = Weight(nextSmaller w.log)

/// An interval whose bounds are nonnegative numbers, represented by their natural logarithm.
///
/// This is useful for weights, likelihoods and probabilities, as it prevents underflow.
[<Struct>]
type WeightInterval =
    val lo: Weight
    val hi: Weight

    new(lo: Weight, hi: Weight, inflate: bool) =
        { lo = if inflate then nextSmaller lo else lo
          hi = if inflate then nextLarger hi else hi }

    new(x: Weight) = WeightInterval(x, x, true)

    member inline this.CheckNaN() : WeightInterval =
        if this.lo.isNaN || this.hi.isNaN then
            WeightInterval(Weight(-infinity), Weight(infinity), false)
        else
            this

    override this.ToString() : string = $"[{this.lo}, {this.hi}]"

    static member Zero =
        WeightInterval(Weight.Zero, Weight.Zero, false)

    static member One =
        WeightInterval(Weight.One, Weight.One, false)

    static member inline Exp(iv: Interval) : WeightInterval =
        WeightInterval(Weight iv.lo, Weight iv.hi, false)

    member inline this.toInterval: Interval =
        let lo =
            if this.lo.isZero () then
                0.0
            elif this.lo.isOne () then
                1.0
            else
                max 0.0 (Interval.nextSmaller (exp this.lo.log))

        let hi =
            if this.hi.isZero () then
                0.0
            elif this.lo.isOne () then
                1.0
            else
                Interval.nextLarger (exp this.hi.log)

        preciseInterval lo hi

    static member inline (+)(x: WeightInterval, y: WeightInterval) : WeightInterval =
        // These special cases were not an issue at the time of writing,
        // but a similar case analysis was necessary for multiplication, so it seems like a good idea.
        let lo =
            if x.lo.isZero () then y.lo
            elif y.lo.isZero () then x.lo
            else nextSmaller (x.lo + y.lo)

        let hi =
            if x.hi.isZero () then y.hi
            elif y.hi.isZero () then x.hi
            else nextLarger (x.hi + y.hi)

        WeightInterval(lo, hi, false)

    static member inline (*)(x: WeightInterval, y: WeightInterval) : WeightInterval =
        // We have to handle the numbers 0.0 and 1.0 special
        // because we don't want to do too much rounding.
        // That would lead to issues in combination with widening: WeightIntervals could be widened to [-infty, infty].

        let a = x.lo * y.lo
        let b = x.lo * y.hi
        let c = x.hi * y.lo
        let d = x.hi * y.hi

        let isExact (z: Weight) = z.isZero () || z.isOne ()

        let xLoExact = isExact x.lo
        let xHiExact = isExact x.hi
        let yLoExact = isExact y.lo
        let yHiExact = isExact y.hi

        let aExact = xLoExact || yLoExact
        let bExact = xLoExact || yHiExact
        let cExact = xHiExact || yLoExact
        let dExact = xHiExact || yHiExact

        let nextSmallerUnless (cond: bool) (z: Weight) = if cond then z else nextSmaller z

        let aDown = nextSmallerUnless aExact a
        let bDown = nextSmallerUnless bExact b
        let cDown = nextSmallerUnless cExact c
        let dDown = nextSmallerUnless dExact d

        let nextLargerUnless (cond: bool) (z: Weight) = if cond then z else nextLarger z

        let aUp = nextLargerUnless aExact a
        let bUp = nextLargerUnless bExact b
        let cUp = nextLargerUnless cExact c
        let dUp = nextLargerUnless dExact d

        let lo = min (min aDown bDown) (min cDown dDown)
        let hi = max (max aUp bUp) (max cUp dUp)
        let result = WeightInterval(lo, hi, false).CheckNaN()

        WeightInterval(result.lo, result.hi, false)

    member inline this.Inverse() : WeightInterval =
        WeightInterval(this.hi.Inverse(), this.lo.Inverse(), false)

    static member inline (/)(x: WeightInterval, y: WeightInterval) : WeightInterval = x * y.Inverse()

let nonnegReals =
    WeightInterval(Weight.Zero, Weight.Infinity, false)

let unitWeightInterval =
    WeightInterval(Weight.Zero, Weight.One, false)

let preciseWeightInterval (lo: Weight) (hi: Weight) = WeightInterval(lo, hi, false)

let weightInterval (lo: Weight) (hi: Weight) = WeightInterval(lo, hi, true)

let pointWeight (x: Weight) = WeightInterval(x, x, true)

let preciseWeight (x: Weight) = WeightInterval(x, x, false)

let toInterval (iv: WeightInterval) = iv.toInterval

let toWeightInterval (iv: Interval) =
    let res = iv.Log()
    WeightInterval(Weight(res.lo), Weight(res.hi), false)

let weightUpTo (x: Weight) = WeightInterval(Weight.Zero, x, false)

let lo (iv: WeightInterval) = iv.lo

let hi (iv: WeightInterval) = iv.hi
