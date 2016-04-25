package org.coursera.progfun.week2


import org.coursera.progfun.week1.Math.abs

import scala.annotation.tailrec


object Curry {
  def sum(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 0 else f(a) + sum(f)(a + 1, b)
  }

  def product(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 1 else f(a) * product(f)(a + 1, b)
  }

  def factorial(n: Int): Int = product(x => x)(1, n)

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, unitValue: Int)(a: Int, b: Int): Int = {
    if (a > b) unitValue
    else combine(f(a), mapReduce(f, combine, unitValue)(a + 1, b))
  }

  def mrSum(f: Int => Int)(a: Int, b: Int): Int = {
    mapReduce(f, (x, y) => (x + y), 0)(a, b)
  }

  def mrProduct(f: Int => Int)(a: Int, b: Int): Int = {
    mapReduce(f, (x, y) => (x * y), 1)(a, b)
  }

  def fixedPoint(f: Double => Double)(initialGuess: Double, tolerance: Double = 0.0001): Double = {
    def isCloseEnough(x: Double, y: Double): Boolean = {
      abs((x - y) / x) / x <= tolerance
    }

    @tailrec
    def loop(guess: Double): Double = {
      val next = f(guess)
      if (isCloseEnough(guess, next)) next
      else loop(next)
    }

    loop(initialGuess)
  }

  def averageDamp(f: Double => Double)(x: Double): Double = (x + f(x)) / 2

  def sqrt(x: Double): Double = {
    fixedPoint(averageDamp(y => x / y))(x)
  }
}
