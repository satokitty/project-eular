package org.projecteular

import org.scalatest.FunSuite

/**
 * 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
 *
 * What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
 */
class Problem5 extends FunSuite {
  // lcm(m,n) = abs(m*n) / gcd(m, n)
  // lcm(l,m,n) = lcm(lcm(m,n),l)

  /*
   * m > n, m > 0, n >= 0
   */
  def gcd(m:Long, n:Long):Long = n match {
    case 0 => m
    case _ => gcd(n, m % n)
  }

  /*
   * nums must be orderd by desc.
   */
  def lcm(nums: Seq[Long]): Long = nums match {
    case a::Nil => a
    case _ => nums.tail.foldLeft(nums.head)((m, n) => m * n / gcd(m, n))
  }

  test("sample") {
    assert(lcm((1L to 10L).reverse) === 2520)
  }

  test("solution") {
    val n = lcm((1L to 20L).reverse)
    (1 to 20).foreach(i => { println(i); assert((n % i) === 0)})
    println("answer is " + n)
  }
}
