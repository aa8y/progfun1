package org.coursera.progfun.week2

abstract class IntSet {
  def contains(x: Int): Boolean

  def incl(x: Int): IntSet

  def union(that: IntSet): IntSet
}

class NonEmpty(n: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean = {
    if (x < n) left contains x
    else if (x > n) right contains x
    else true
  }

  def incl(x: Int): IntSet = {
    if (x < n) left incl x
    else if (x > n) right incl x
    else this
  }

  def union(that: IntSet): IntSet = ((left union right) union that) incl n

  override def toString: String = s"{ $left $n $right }"
}

object Empty extends IntSet {
  def contains(x: Int): Boolean = false

  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)

  def union(that: IntSet): IntSet = that

  override def toString: String = "."
}
