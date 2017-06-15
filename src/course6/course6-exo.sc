val b = (1 to 10).toList

def myFilter(l: List[Int], f: Int => Boolean): List[Int] = l match {
  case Nil => Nil
  case head :: tail if f(head) => myFilter(tail, f)
  case head :: tail => head :: myFilter(tail, f)
}

myFilter(b, _ % 2 == 0)

def myPartition(l: List[Int], f: Int => Boolean): (List[Int], List[Int]) = l match {
  case Nil => (List.empty[Int], List.empty[Int])
  case head :: tail if f(head) => {
    val (left, right) = myPartition(tail, f)
    (head :: left, right)
  }
  case head :: tail => {
    val (left, right) = myPartition(tail, f)
    (left, head :: right)
  }
}
myPartition(b, _ % 2 == 0)


def myPartition2(l: List[Int], f: Int => Boolean): (List[Int], List[Int]) =
  l.foldRight((List.empty[Int], List.empty[Int])) {
    case (e, (l, r)) if f(e) => (e :: l, r)
    case (e, (l, r)) => (l, e :: r)
  }

myPartition2(b, _ % 2 == 0)