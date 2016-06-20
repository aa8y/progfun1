package org.coursera.progfun.week1

import scala.annotation.tailrec

object Run extends App {
  import Math._

  println("Factorial of:")
  Seq(0, 5).foreach { n =>
    println(s"$n = ${factorial(n)}")
  }
  println()

  println("GCD of:")
  Seq((0, 14), (21, 14), (14, 21)).foreach {
    case (x: Int, y: Int) => println(s"($x, $y) = ${gcd(x, y)}")
  }
  println()

  println("Square root of:")
  Seq(2, 1e-6, 1e60).foreach { n =>
    println(s"$n = ${sqrt(n)}")
  }
  println()
}

object Math {
  def abs(n: Double) = if (n < 0) -n else n

  def factorial(n: Int): Int = {
    @tailrec
    def loop(acc: Int, n: Int): Int = {
      if (n == 0) acc
      else loop(acc * n, n - 1)
    }
    loop(1, n)
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
