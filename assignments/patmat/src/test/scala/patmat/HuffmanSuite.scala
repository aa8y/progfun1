package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}
  trait ExampleTree {
    val egCode = Fork(
      left = Leaf('a', 8),
      right = Fork(
        left = Fork(
          left = Leaf('b', 3),
          right = Fork(
            left = Leaf('c', 1),
            right = Leaf('d', 1),
            chars = List('c', 'd'),
            weight = 2
          ),
          chars = List('b', 'c', 'd'),
          weight = 5
        ),
        right = Fork(
          left = Fork(
            left = Leaf('e', 1),
            right = Leaf('f', 1),
            chars = List('e', 'f'),
            weight = 2
          ),
          right = Fork(
            left = Leaf('g', 1),
            right = Leaf('h', 1),
            chars = List('g', 'h'),
            weight = 2
          ),
          chars = List('e', 'f', 'g', 'h'),
          weight = 4
        ),
        chars = List('b', 'c', 'd', 'e', 'f', 'g', 'h'),
        weight = 9
      ),
      chars = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'),
      weight = 17
    )
  }
  trait PersonalTree {
    val tinyTree = Fork(Leaf('a', 1), Leaf('b', 1), List('a', 'b'), 2)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times should group and count") {
    val chars = string2Chars("aaabcc")
    assert(times(chars) === List(('b', 1), ('a', 3), ('c', 2)))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode with example tree should do single character decoding") {
    new ExampleTree {
      assert(decode(egCode, List(0)) === List('a'))
      assert(decode(egCode, List(1, 0, 0)) === List('b'))
    }
  }

  test("decode with example tree should do two character decoding") {
    new ExampleTree {
      assert(decode(egCode, List(0, 1, 0, 0)) === List('a', 'b'))
    }
  }

  test("decode with example tree should decode 10001010 to bac") {
    new ExampleTree {
      assert(decode(egCode, List(1, 0, 0, 0, 1, 0, 1, 0)) === List('b', 'a', 'c'))
    }
  }

  test("convert() should work for a tiny tree") {
    new PersonalTree {
      assert(convert(tinyTree) === List(('a', List(0)), ('b', List(1))))
    }
  }

  test("convert() should work for a complex tree") {
    new ExampleTree {
      val codeTree = convert(egCode).toMap
      assert(codeTree('b') === List(1, 0, 0))
      assert(codeTree('e') === List(1, 1, 0, 0))
      assert(codeTree('g') === List(1, 1, 1, 0))
    }
  }

  test("encode() and quickEncode() should give the same result") {
    new ExampleTree {
      val bac = "bac".toList
      assert(encode(egCode)(bac) === quickEncode(egCode)(bac))
      val baccha = "baccha".toList
      assert(encode(egCode)(baccha) === quickEncode(egCode)(baccha))
    }
  }
}
