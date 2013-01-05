package satokitty.projecteular

import org.scalatest.FunSuite

/**
 * If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
 *
 * Find the sum of all the multiples of 3 or 5 below 1000.
 */
class Problem1 extends FunSuite {
  def f(i: Int) = i % 3 == 0 || i % 5 == 0

  test("sample") {
    assert((0 until 10).filter(f).sum === 23)
  }

  test("solution") {
    println((0 until 1000).filter(f).sum)
  }
}
