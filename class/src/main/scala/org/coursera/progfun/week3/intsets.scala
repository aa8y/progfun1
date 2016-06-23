package org.coursera.progfun.week3

object intsets {
}

abstract class IntSet {
  def contains(x: Int): Boolean

  def incl(x: Int): IntSet

  def size: Int

  def union(other: IntSet): IntSet
}

class NonEmpty(val n: Int, val left: IntSet = Empty, val right: IntSet = Empty) extends IntSet {
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

  def size: Int = left.size + 1 + right.size

  def union(other: IntSet): IntSet = ((left union right) union other) incl n

  override def equals(other: Any): Boolean = other match {
    case o: NonEmpty => o.n == this.n && o.left == this.left && o.right == this.right
    case _ => false
  }

  override def toString: String = s"{ $left $n $right }"
}

class Empty extends IntSet {
  def contains(x: Int): Boolean = false

  def incl(x: Int): IntSet = new NonEmpty(x)

  def size: Int = 0

  def union(other: IntSet): IntSet = other

  override def equals(other: Any): Boolean = other match {
    case o: Empty => true
    case _ => false
  }

  override def toString: String = "."
}

object Empty extends Empty
