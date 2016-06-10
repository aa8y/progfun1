package recfun

import java.lang.{ IndexOutOfBoundsException, IllegalArgumentException }

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
    if (r < 0 || c < 0) throw new IndexOutOfBoundsException("Row/column indices cannot be negative.")
    if (c > r) throw new IndexOutOfBoundsException("Column index cannot be greater than row index.")

    if (r == 0) 1
    else if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }
  
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def loop(chars: List[Char], count: Int = 0): Boolean = {
      if (count < 0) false
      else if (chars.isEmpty && count == 0) true
      else if (chars.isEmpty && count > 0) false
      else {
        if (chars.head == '(') loop(chars.tail, count + 1)
        else if (chars.head == ')') loop(chars.tail, count - 1)
        else loop(chars.tail, count)
      }
    }

    loop(chars)
  }
  
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.count(_ < 0) > 0) {
      throw new IllegalArgumentException("Coin denominations cannot be negative.")
    }
    val denoms = coins.distinct

    if (money < 0 || denoms.isEmpty) 0
    else if (money == 0) 1
    else countChange(money, denoms.tail) + countChange(money - denoms.head, denoms)
  }
}
