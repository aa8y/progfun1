package org.coursera.progfun.week2


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
}
