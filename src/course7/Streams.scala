package course7

/**
  * date: 26.04.17
  *
  * @author Lucy Linder <lucy.derlin@gmail.com>
  */

object StreamsApp extends App {

  // fibonacci: 0, 1, 1, 2, 3, 5, 8, 13, 21, 34
  def addStreams(s1: Stream[Int], s2: Stream[Int]): Stream[Int] =
    (s1.head + s2.head) #:: addStreams(s1.tail, s2.tail)

  def fibo1: Stream[Int] = 0 #:: 1 #:: addStreams(fibo1, fibo1.tail)


  def fibo2: Stream[Int] = 0 #:: 1 #:: fibo2.zip(fibo2.tail).map {
    { n => n._1 + n._2 }
  }

  println(fibo1.take(10).toList)
  println(fibo2.take(10).toList)

  // prime numbers
  def primes(): Stream[Int] = {
    def _primes(s: Stream[Int]): Stream[Int] = s.head #:: s.tail.filter(elem => elem % s.head != 0)

    _primes(Stream.from(2))
  }

  println(primes.take(10).toList)
}
