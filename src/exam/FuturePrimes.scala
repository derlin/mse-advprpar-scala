package exam


import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Success, Failure}

object Primes {
  // --- given
  def isPrime(i: Int): Boolean = i match {
    case i if i <= 1 => false
    case 2 => false
    case _ => !((2 to i - 1)).exists(x => i % x == 0)
  }

  def primes(from: Int, to: Int): List[Int] = {
    for (i <- (from to to).toList if isPrime(i)) yield i
  }

  val granularity = 10000

}

object FuturePrimesSimple extends App {

  import Primes._

  val f1 = Future {primes(0, granularity)}
  val f2 = Future {primes(granularity, 2 * granularity)}
  val f3 = Future {primes(2 * granularity, 3 * granularity)}

  val results = for (r1 <- f1; r2 <- f2; r3 <- f3) yield r1.length + r2.length + r3.length

  println(Await.result(results, 5 minutes))
}

object FuturePrimesSimple2 extends App {

  import Primes._

  val f1 = Future {primes(0, granularity)}
  val f2 = Future {primes(granularity, 2 * granularity)}
  val f3 = Future {primes(2 * granularity, 3 * granularity)}

  val results = for (r1 <- f1; r2 <- f2; r3 <- f3) yield r1.length + r2.length + r3.length

  results onComplete  {
    case Success(x) => println(s"count: $x")
    case Failure(e) => println(s"error: $e")
  }
  Await.ready(results, 5 minutes)
  Thread.sleep(100) // be sure println has time to complete
}

object FuturePrimes extends App {

  import Primes._

  val futures = (0 until 3).map {
    i => Future {primes(granularity * i, granularity * (i + 1)).length}
  }

  val results = Future.fold(futures)(0)(_ + _)

  println(Await.result(results, 5 minutes))
}
