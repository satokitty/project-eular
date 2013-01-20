package org.projecteular

import org.scalatest.FunSuite
import java.math.BigInteger

/**
 * By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
 *
 * What is the 10 001st prime number?
 */
class Problem7 extends FunSuite {
  def from(n: Long): Stream[Long] = Stream.cons(n, from(n + 1))

  def sieve(xs: Stream[Long]): Stream[Long] = Stream.cons(xs.head, sieve(xs.tail.filter(_ % xs.head != 0)))


  test("find prime number") {
    assert(sieve(from(2)).take(6).reverse.head === 13)
  }

  test("solution") {
//    println(sieve(from(2)).take(10001).reverse.head)
//    OutOfMemoryError!! hmm...
    def prime(i: BigInteger, count: Int): BigInteger = count match {
      case 0 => i
      case _ => prime(i.nextProbablePrime(), count - 1)
    }

    println(prime(BigInteger.valueOf(2), 10000))
  }
}
