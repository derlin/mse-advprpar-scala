import scala.annotation.tailrec


def fibo(n: Int): Int = {
  require(n >= 0, "n positif")
  n match {
    case 0 => 0
    case 1 => 1
    case n => fibo(n - 1) + fibo(n - 2)
  }
}

def fiborec(n: Int): Int = {

  @tailrec
  def fib(a: Int, b: Int, i: Int): Int = {
    if (i >= n) a
    else fib(b, a + b, i + 1)
  }

  fib(0, 1, 0)
}

fibo(20)
fiborec(20)

// ---------------------------

{
  sealed abstract class BinaryTree {
    def leafSum: Int = this match {
      case Leaf(n) => n
      case Node(left, right) => left.leafSum + right.leafSum
    }

    def leafMin: Int = this match {
      case Leaf(n) => n
      case Node(left, right) =>
        val lvalue = left.leafMin
        val rvalue = right.leafMin
        if (lvalue < rvalue) lvalue else rvalue
    }
  }

  case class Leaf(value: Int) extends BinaryTree
  case class Node(left: BinaryTree, right: BinaryTree) extends BinaryTree

  val tree = Node(Node(Node(Leaf(2), Leaf(3)), Leaf(-1)), Node(Node(Leaf(10), Leaf(1)), Leaf(45)))

  println("min: " + tree.leafMin)
  println("sum: " + tree.leafSum)
}

// --------------------------

def last[T](ls: List[T]): T = ls match {
  case x :: y :: xs => last(y :: xs)
  case x :: xs => x
  case Nil => sys.error("empty list")
}


def init[T](ls: List[T]): List[T] = ls match {
  case x :: y :: xs => x :: init(y :: xs)
  case x :: xs => xs
  case Nil => Nil
}

def reverse[T](ls: List[T]): List[T] = ls match {
  case x :: xs => reverse(xs) :+ x
  case Nil => Nil
}

def concat[T](l1: List[T], l2: List[T]): List[T] =
  l1.foldRight(l2) { case (elt, acc) => elt :: acc }

def take[T](ls: List[T], n: Int): List[T] = ls match {
  case x :: xs if n > 0 => x :: take(xs, n - 1)
  case _ if n <= 0 => Nil
  case Nil => Nil
}

def drop[T](ls: List[T], n: Int): List[T] = ls match {
  case x :: xs if n > 0 => drop(xs, n - 1)
  case xs if n <= 0 => xs
  case Nil => Nil
}

def any[T](ls: List[T], f: T => Boolean): Boolean = ls match {
  case x :: xs if f(x) => true
  case x :: xs => any(xs, f)
  case Nil => false
}

def every[T](ls: List[T], f: T => Boolean): Boolean = ls match {
  case x :: xs if !f(x) => false
  case x :: xs => every(xs, f)
  case Nil => true
}

val ls = (1 to 10).toList
last(ls)
init(ls)

take(ls, 13)
take(ls, 2)

drop(ls, 13)
drop(ls, 2)

concat(1 :: 2 :: Nil, 3 :: 4 :: Nil)

every(List(), 3 > (_: Int))
any(List(1, 2, 3), 0 > (_: Int))
any(List(1, 2, 3), 2 > (_: Int))

// --------------------

def lengthStrings(ls: List[String]) = ls.map(_.length)


def dup[T](elt: T, n: Int): List[T] = (0 until n).foldLeft(List.empty[T]) {
  (acc, _) => elt :: acc
}

def dup2[T](elt: T, n: Int): List[T] = {
  require(n >= 0)
  n match {
    case 0 => Nil
    case _ => elt :: dup2(elt, n - 1)
  }
}

def dot(l1: List[Int], l2: List[Int]): List[Int] = {
  (l1 zip l2).map(t => t._1 * t._2)
}


def flattenList(ls: List[Any]): List[Any] = ls match {
  case (x :: xs) :: ys => x :: flattenList(xs) ++ flattenList(ys)
  case Nil :: ys => flattenList(ys)
  case elt :: ys => elt :: flattenList(ys)
  case Nil => Nil
}

flattenList(List(List(1, 1), 2, List(3, List(5, 8)))) // List(1,1,2,3,5,8)

// ---------------------

def addStream(s1: Stream[Int], s2: Stream[Int]): Stream[Int] =
  (s1.head + s2.head) #:: addStream(s1.tail, s2.tail)

def fibostream: Stream[Int] = 0 #:: 1 #:: addStream(fibostream, fibostream.tail)

def fibostream2: Stream[Int] = 0 #:: 1 #:: fibostream2.zip(fibostream2.tail).map(t => t._1 + t._2)

fibostream.take(10).toList
fibostream2.take(10).toList

def intStream(n: Int): Stream[Int] = Stream.cons(n, intStream(n + 1))

intStream(0).take(10).toList

def primes: Stream[Int] =
  Stream.cons(2, intStream(3).filter(i => i % primes.head != 0))

primes.take(10).toList
