package org.coursera.progfun.week3

object intsets {
}

abstract class IntSet {
  def contains(x: Int): Boolean

  def incl(x: Int): IntSet

  def size: Int

  def intersect(other: IntSet): IntSet

  def union(other: IntSet): IntSet
}

class NonEmpty(val n: Int, val left: IntSet = Empty, val right: IntSet = Empty) extends IntSet {
  def contains(x: Int): Boolean = {
    if (x < n) left contains x
    else if (x > n) right contains x
    else true
  }

  def incl(x: Int): IntSet = {
    if (x < n) new NonEmpty(n, left incl x, right)
    else if (x > n) new NonEmpty(n, left, right incl x)
    else this
  }

  def intersect(other: IntSet): IntSet = other match {
    case that: NonEmpty => 
      val inter = (left intersect right) intersect that
      if (that.contains(n)) inter incl n else inter
    case _ => Empty
  }

  def size: Int = left.size + 1 + right.size

  def union(other: IntSet): IntSet = ((left union right) union other) incl n

  override def equals(other: Any): Boolean = other match {
    case that: NonEmpty =>
      if (this.n > that.n) {
        this.left == new NonEmpty(that.n, that.left, Empty) && 
        new NonEmpty(this.n, Empty, this.right) == that.right
      } else if (this.n < that.n) {
        new NonEmpty(this.n, this.left, Empty) == that.left && 
        this.right == new NonEmpty(that.n, Empty, that.right)
      } else that.left == this.left && that.right == this.right
    case _ => false
  }

  override def toString: String = s"{ $left $n $right }"
}

class Empty extends IntSet {
  def contains(x: Int): Boolean = false

  def incl(x: Int): IntSet = new NonEmpty(x)

  def intersect(other: IntSet): IntSet = this

  def size: Int = 0

  def union(other: IntSet): IntSet = other

  override def equals(other: Any): Boolean = other match {
    case o: Empty => true
    case _ => false
  }

  override def toString: String = "."
}

object Empty extends Empty
