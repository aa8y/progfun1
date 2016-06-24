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

  it should "return itself when intersected with any other set." in {
    (Empty intersect Empty) should be (Empty)
    (Empty intersect s1) should be (Empty)
  }

  "A non-empty set" should "return the same non-empty set when unioned with an empty set." in {
    (s1 union Empty) should be (s1)
  }

  it should "return an empty set when intersected with an empty set." in {
    (s1 intersect Empty) should be (Empty)
  }

  it should "return an empty set when intersected with a non-empty set with no common elements." in {
    (s1 intersect s2) should be (Empty)
  }

  it should "be equal to another non-empty set even when the insertion order is different." in {
    val s1 = ((new NonEmpty(1) incl 2) incl 3)
    val s2 = ((new NonEmpty(2) incl 1) incl 3)
    val s3 = ((new NonEmpty(3) incl 2) incl 1)

    (s1) should be (s2)
    (s2) should be (s3)
    (s3) should be (s1)
  }
}
