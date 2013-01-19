package org.projecteular

import org.scalatest.FunSuite

/**
 * The sum of the squares of the first ten natural numbers is,
 *
 * 12 + 22 + ... + 102 = 385
 * The square of the sum of the first ten natural numbers is,
 *
 * (1 + 2 + ... + 10)2 = 552 = 3025
 * Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025  385 = 2640.
 *
 * Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
 */
class Problem6 extends FunSuite {
  def square(x: Long): Long = x * x
  test("sample") {
    def a = (1 to 10).foldLeft(0L)((accum, i) => accum + square(i))
    def b = square((1 to 10).sum)
    assert(b - a === 2640)
  }

  test("solution") {
    def a = (1L to 100L).foldLeft(0L)((accum, i) => accum + square(i))
    def b = square((1L to 100L).sum)
    println(b - a)
  }
}
