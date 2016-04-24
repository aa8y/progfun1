package org.coursera.progfun.week2


import org.scalatest.{FlatSpec, Matchers}


class CurryTest extends FlatSpec with Matchers {
  "sum()" should "work for an identity function." in {
    Curry.sum(x => x)(1, 5) should be (15)
  }

  it should "work for a square function." in {
    Curry.sum(x => x * x)(1, 3) should be (14)
  }

  "product()" should "work for an identity function." in {
    Curry.product(x => x)(1, 5) should be (120)
  }

  it should "work for a square function." in {
    Curry.product(x => x * x)(1, 3) should be (36)
  }

  "factorial()" should "calculate the factorial of a given number." in {
    Curry.factorial(5) should be (120)
  }

  "mrSum()" should "return the same value as sum for the same arguments." in {
    assert(Curry.mrSum(x => x)(1, 5) == Curry.sum(x => x)(1, 5))
  }

  "mrProduct()" should "return the same value as product() for the same arguments." in {
    assert(Curry.mrProduct(x => x)(1, 5) == Curry.product(x => x)(1, 5))
  }
}
