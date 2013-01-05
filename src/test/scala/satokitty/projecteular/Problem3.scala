package satokitty.projecteular

import org.scalatest.FunSuite

/**
 * The prime factors of 13195 are 5, 7, 13 and 29.
 *
 * What is the largest prime factor of the number 600851475143 ?
 */
class Problem3 extends FunSuite {
  lazy val num: Stream[Long] = {
    def loop(i: Long): Stream[Long] = i #:: loop(i + 1)
    loop(2)
  }

  def f(accum: Long, primes: Stream[Long], results: List[Long]): Long = accum match {
    case 1 => results.head
    case _ => accum % primes.head match {
      case 0 => f(accum / primes.head, primes, if (results.contains(primes.head)) results else primes.head :: results)
      case _ => f(accum, primes.tail.filterNot(_ % primes.head == 0), results)
    }
  }

  test("sample") {
    assert(f(13195, num.takeWhile(_ <= 13195), Nil) === 29)
  }

  test("solution") {
    println(f(600851475143L, num.takeWhile(_ <= 600851475143L), Nil))
  }
}
