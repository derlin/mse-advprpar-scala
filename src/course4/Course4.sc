

def patFoo(x: Any): Boolean = {
  // beware: if we enter in one case, we won't test for the
  // next --> use guards
  x match {
    case x: Int => (x % 4 == 0)
    case x: Char => x.isUpper
    case x: Boolean => true
    case _ => false
  }
}


def insert(x: Int, list: List[Int]): List[Int] = {
  if (list.isEmpty) x :: Nil
  else if (x < list.head) x :: list
  else list.head :: insert(x, list.tail)
}


def insertMatch(x: Int, list: List[Int]): List[Int] = {
  list match {
    case Nil => x :: Nil
    case head :: tail => {
      if (x < head) x :: list
      else head :: insertMatch(x, tail)
    }
  }
}