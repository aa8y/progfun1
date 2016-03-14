package recfun


import common._

import scala.annotation.tailrec


object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (r < 0) throw new IndexOutOfBoundsException(s"Row index cannot be negative. Given: $r.")
    if (c < 0) throw new IndexOutOfBoundsException(s"Column index cannot be negative. Given: $c.")
    if (c > r) throw new IndexOutOfBoundsException(s"Column index cannot be greater than row index. Given: $c.")

    if (r <= 1 || c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def loop(l: List[Char], count: Int = 0): Boolean = {
      if (count < 0) false
      else if (l.isEmpty) count == 0
      else if (l.head == '(') loop(l.tail, count + 1)
      else if (l.head == ')') loop(l.tail, count - 1)
      else loop(l.tail, count)
    }
    loop(chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.filter(_ <= 0).nonEmpty) {
      throw new IllegalArgumentException(s"Coin denominations can neither be negative nor zero. Given: ${coins.mkString(", ")}")
    }
    val denoms = coins.distinct

    if (money == 0) 1
    else if (money < 0 || denoms.isEmpty) 0
    else countChange(money - denoms.head, denoms) + countChange(money, denoms.tail)
  }
}
