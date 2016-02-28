package org.coursera.progfun.week1


import scala.annotation.tailrec


object Math {
  def abs(n: Double) = if (n < 0) -n else n

  def factorial(n: Int): Long = {
    if (n == 0) 1
    else n * factorial(n - 1)
  }

  @tailrec
  def gcd(x: Int, y: Int): Int = {
    if (y == 0) x
    else gcd(y, x % y)
  }

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
