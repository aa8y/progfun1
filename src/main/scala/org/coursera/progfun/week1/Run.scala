package org.coursera.progfun.week1


import org.coursera.progfun.week1.Math._


object Week1Run extends App {
  println("Factorial of:")
  Seq(0, 5).foreach { n =>
    println(s"$n = ${factorial(n)}")
  }
  println()

  println("GCD of:")
  Seq((0, 14), (21, 14), (14, 21)).foreach {
    case (x: Int, y: Int) => println(s"($x, $y) = ${gcd(x, y)}")
  }
  println()

  println("Square root of:")
  Seq(2, 1e-6, 1e60).foreach { n =>
    println(s"$n = ${sqrt(n)}")
  }
  println()
}
