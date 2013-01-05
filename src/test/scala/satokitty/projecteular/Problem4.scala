package satokitty.projecteular

import org.scalatest.FunSuite

/**
 * A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 99.
 *
 * Find the largest palindrome made from the product of two 3-digit numbers.
 */
class Problem4 extends FunSuite {

  def isPalindromic(num: Int): Boolean = {
    val numStr = String.valueOf(num)
    numStr == numStr.reverse
  }

  test("sample") {
    assert(isPalindromic(0))
    assert(isPalindromic(1221))
    assert(isPalindromic(12321))

    assert(!isPalindromic(1212))
  }

  test("solution") {
    val max = (900 to 999).combinations(2).filter(list => isPalindromic(list(0) * list(1))).maxBy(list => list(0) * list(1))
      .foldLeft(1)(_ * _)
    println(max)
  }

}
