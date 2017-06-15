def last[T](list: List[T]): T = list match {
  case x :: Nil => x
  case x :: xs => last(xs)
  case _ => sys.error("no last element in an empty list")
}


def init[T](list: List[T]): List[T] = list match {
  case _ :: Nil | Nil => Nil
  case x :: xs => x :: init(xs)
}

def reverse[T](list: List[T]): List[T] = list match {
  case Nil => Nil
  case x :: xs => reverse(xs) ++ (x :: Nil)
}

def concat[T](list1: List[T], list2: List[T]): List[T] =
  if (list2.isEmpty) list1 else {
    list1 match {
      case Nil => list2
      case x :: xs => x :: concat(xs, list2)
    }
  }


def take[T](list: List[T], n: Int): List[T] = list match {
  case x :: xs if (n > 0) => x :: take(xs, n - 1)
  case _ => Nil
}

def drop[T](list: List[T], n: Int): List[T] = list match {
  case x :: xs if (n > 0) => drop(xs, n - 1)
  case any => any
}


def apply[T](list: List[T], n: Int): T = list match {
  case Nil => sys.error("IndexOutOfBounds")
  case x :: _ if (n == 0) => x
  case x :: xs => apply(xs, n - 1)
}

def any[T](p: T => Boolean)(l: List[T]): Boolean = l match {
  case Nil => false
  case x :: xs if (!p(x)) => any(p)(xs)
  case _ => true
}

def every[T](p: T => Boolean)(l: List[T]): Boolean = l match {
  case Nil => true
  case x :: xs if (p(x)) => any(p)(xs)
  case _ => false
}

val l1 = 4 :: 3 :: 24 :: 10 :: 1 :: 0 :: Nil
val l2 = 1 :: 2 :: 3 :: 4 :: Nil
concat(Nil, Nil)
any(4 == (_ : Int))(l1)
every(40 > (_ : Int))(l1)

