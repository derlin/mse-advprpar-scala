package course2

import scala.annotation.tailrec

/**
  * date: 01.03.17
  *
  * @author Lucy Linder <lucy.derlin@gmail.com>
  */

object Assignment2 extends App {

  // exo: tail fact
  @tailrec
  def fact(x: Int, x0: Int = 1): Int = {
    if (x == 0) x0 else fact(x - 1, x0 * x)
  }

  //  println(fact(5))


  def fibo(x: Int, x0: Int = 1): Int = {
    if (x == 0) 0 else if (x == 1) 1 else fibo(x - 1) + fibo(x - 2)
  }

  def fiboTail(x: Int): Int = {
    @tailrec
    def fiboTailRec(x1: Int, x2: Int, cnt: Int): Int = {
      if (cnt >= x) x1 + x2 // or: if(cnt > x) x1
      else fiboTailRec(x2, x1 + x2, cnt + 1)
    }

    fiboTailRec(0, 1, 2)
  }

  //  println(fibo(10))
  //  println(fiboTail(10))

  def sum(f: Int => Int, a: Int, b: Int) = {
    def iter(a: Int, acc: Int): Int = {
      if (a > b) acc else iter(a + 1, acc + f(a))
    }

    iter(a, 0)
  }

  //  println(sum(x => x, 1, 5))


  def product(f: Int => Int)(a: Int)(b: Int) = {
    def iter(a: Int)(acc: Int): Int = {
      if (a > b) acc else iter(a + 1)(acc * f(a))
    }

    iter(a)(1)
  }

  def genericFunc(op: (Int, Int) => Int, acc: Int, f: Int => Int, a: Int, b: Int): Int = {
    def iter(a: Int, acc: Int): Int = {
      if (a > b) acc else iter(a + 1, op(acc, f(a)))
    }

    iter(a, acc)
  }

  def genericSum = genericFunc((_ + _), 0, _: Int => Int, _: Int, _: Int)

  def genericProduct = genericFunc((_ * _), 1, _: Int => Int, _: Int, _: Int)


  println(genericSum(x => x, 3, 4))
  println(genericProduct(x => x, 3, 4))
}

