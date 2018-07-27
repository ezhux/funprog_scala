package state

import scala.util.Random
import Math._

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object State {

  //Exercise 6.1

  def nonNegative(rng: RNG): (Int, RNG) = {
    rng.nextInt match {
      case (i, n) if i < 0 => (Math.abs(i), n)
      case (i, n) if i == Int.MinValue => (Math.abs(i+1), n)
      case (i, n) => (i, n)
    }
  }

  // Exercise 6.2
  def double(rng: RNG): (Double, RNG) = {
    nonNegative(rng) match {
      case (i, r) => (i.toDouble / Int.MaxValue, r)
    }
  }

//  Exercise 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val d = double(rng)
    val i = rng.nextInt
    ((i._1, d._1), d._2)
  }


  // Exercise 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {

    def internal(remaining: Int, rng: RNG, list: List[Int]):(List[Int], RNG) = {
      if (remaining == 0)
        (list, rng)
      else {
        val ni = rng.nextInt
        internal(remaining - 1, ni._2, ni._1 :: list)
      }
    }
    internal(count, rng, List.empty[Int])
  }

  type Rand[+A] = RNG => (A, RNG)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegative)(i => i - i % 2)

  // Exercise 6.5
  def double_with_map(): Rand[Double] = {
    map(nonNegative)(i => i.toDouble / Int.MaxValue)
  }

  // Exercise 6.6
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
      rng => {
        val (a, rng2) = ra(rng)
        val (b, rng3) = rb(rng2)
        (f(a, b), rng3)
      }
  }

  // Exercise 6.8
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, rng1) = f(rng)
      g(a)(rng1)
    }
  }

//  def nonNegativeLessThan(n: Int): Rand[Int] = {
//
//  }

}
