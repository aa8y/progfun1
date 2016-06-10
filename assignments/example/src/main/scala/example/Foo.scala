package example

object Foo extends App {
  def bar(s: String): String = {
    if (s.size == 0) "empty string"
    else if (s.size == 1) s + s
    else s.head + bar(s.tail)
  }

  println(bar(""))
  println(bar("s"))
  println(bar("soom"))
}
