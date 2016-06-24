package org.coursera.progfun.week1

import scala.math.round

import org.scalatest.{FlatSpec, Matchers}

class MathTest extends FlatSpec with Matchers {

  "abs()" should "return the absolute value of a number." in {
    Math.abs(-5) should be (5)
    Math.abs(5) should be (5)
  }

  "gcd()" should "be 1 for two prime numbers." in {
    Math.gcd(3, 7) should be (1)
  }

  it should "be the number itself for two same numbers." in {
    Math.gcd(100, 100) should be (100)
  }

  "factorial()" should "return 1 for 0." in {
    Math.factorial(0) should be (1)
  }

  it should "return all numbers multiplied till n for n." in {
    Math.factorial(3) should be (6)
  }

  "sqrt()" should "return the square root of a number." in {
    round(Math.sqrt(4)) should be (2)
  }
}
