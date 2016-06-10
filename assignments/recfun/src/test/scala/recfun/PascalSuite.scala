package recfun

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PascalSuite extends FunSuite {
  import Main.pascal

  test("pascal: col = 0, row = 2") {
    assert(pascal(0, 2) === 1)
  }

  test("pascal: col = 1, row = 2") {
    assert(pascal(1, 2) === 2)
  }

  test("pascal: col = 1, row = 3") {
    assert(pascal(1, 3) === 3)
  }

	test("pascal: negative column index is invalid.") {
    intercept[IndexOutOfBoundsException] {
      pascal(-1, 0)
    }
  }

  test("pascal: negative row index is invalid") {
    intercept[IndexOutOfBoundsException] {
      pascal(0, -1)
    }
  }

  test("pascal: column index cannot be greater than row index.") {
    intercept[IndexOutOfBoundsException] {
      pascal(5, 4)
    }
  }
}
