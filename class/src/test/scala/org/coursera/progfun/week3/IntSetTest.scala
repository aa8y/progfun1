package org.coursera.progfun.week3

import org.scalatest.{FlatSpec, Matchers}

class IntSetTest extends FlatSpec with Matchers {
  private val s1 = new NonEmpty(1)
  private val s2 = new NonEmpty(2)

  "An empty set" should "have 0 size." in {
    Empty.size should be (0)
  }

  it should "increase in size by 1 when one element is included." in {
    (Empty incl 100).size should be (1)
  }

  it should "return an empty set when unioned with another empty set." in {
    (Empty union Empty) should be (Empty)
  }

  it should "return the same non-empty set when unioned with a non-empty set." in {
    (Empty union s2) should be (s2)
  }

  "A non-empty set" should "return the same non-empty set when unioned with an empty set." in {
    (s1 union Empty) should be (s1)
  }
}
