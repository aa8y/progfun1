package org.coursera.progfun.week1


import scala.annotation.tailrec


object Math {
  def abs(n: Double) = if (n < 0) -n else n

  @tailrec
  def sqrtIter(guess: Double, n: Double): Double = {
    if (isGoodEnough(guess, n)) guess
    else sqrtIter(improve(guess, n), n)
  }

  def isGoodEnough(guess: Double, n: Double): Boolean = {
    abs(guess * guess - n) / n < 0.001
  }

  def improve(guess: Double, n: Double): Double = {
    (guess + n / guess) / 2
  }

  def sqrt(n: Double): Double = sqrtIter(1.0, n)
}
