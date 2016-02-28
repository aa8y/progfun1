package org.coursera.progfun.week1


import scala.annotation.tailrec


object Math {
  def abs(n: Double) = if (n < 0) -n else n

  def sqrt(n: Double): Double = {
    @tailrec
    def sqrtIter(guess: Double): Double = {
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))
    }

    def isGoodEnough(guess: Double): Boolean = {
      abs(guess * guess - n) / n < 0.001
    }

    def improve(guess: Double): Double = {
      (guess + n / guess) / 2
    }

    sqrtIter(1.0)
  }
}
