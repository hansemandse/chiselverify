package chiselverify.assembly

import scala.math.{ceil, log10, pow}
import scala.util.Random

object RandomHelpers {

  // choose a random element from a sequence
  def randomSelect[T](xs: Seq[T], rng: Random): T = xs.apply(rng.nextInt(xs.length))

  // Generate random BigInt from bitwidth
  def rand(n: Int, rng: Random): BigInt = BigInt(n, rng)

  // Generate a random BigInt from a maximum value
  def rand(max: BigInt, rng: Random): BigInt = {
    val r = rand((if(max > 1024) (max-1) else max).bitCount, rng)
    if (r > max) rand(max, rng) else r
  }

  // generate a random BigInt inside a BigRange
  def rand(r: BigRange, rng: Random): BigInt = rand(r.length, rng) + r.min

  // generate a random BigInt from bitwidth
  def rand(w: Width, rng: Random): BigInt = rand(BigRange(w), rng)

  def pow2(x: Int): BigInt = BigInt(pow(2, x).toLong & 0xFFFFFFFFFFFFFFFFL)

  def log2Ceil(x: Int): Int = ceil(log10(x) / log10(2)).toInt

  // check whether the given value can be represented using the passed width
  def fits(value: BigInt)(w: Width): Boolean = BigRange(w).contains(value)

  // divide a number into two pieces fitting the two passed bitwidths
  def randSplit(value: BigInt, rng: Random)(w1: Width, w2: Width): (BigInt, BigInt) = {
    require(value > 0)
    val splitter = rng.nextDouble()
    val v1 = BigInt((splitter * value.toLong).toLong)
    val v2 = BigInt(((1-splitter) * value.toLong).toLong)
    if (fits(v1)(w1) && fits(v2)(w2)) (v1,v2) else randSplit(value, rng)(w1,w2)
  }

  // range for BigInts. The range is exclusive at the max.
  case class BigRange(min: BigInt, max: BigInt) {
    val length = max - min

    def contains(that: BigInt): Boolean = (that >= min) && (that < max)
  }

  object BigRange {
    // create a BigRange based upon a bitwidth
    def apply(w: Width): BigRange = {
      w match {
        case Signed(w) => BigRange(-pow2(w - 1), pow2(w - 1))
        case Unsigned(w) => BigRange(0, pow2(w))
      }
    }

    def apply(r: Range): BigRange = BigRange(r.min, r.max)

    // create a BigRange containing a single number
    def apply(v: Int): BigRange = BigRange(v, v + 1)
  }

}
