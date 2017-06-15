def isPrime(i: Int): Boolean = i match {
  case i if i <= 1 => false
  case 2 => false
  case _ => !((2 to i - 1)).exists(x => i % x == 0)
}

def primeSum(max: Int): List[(Int, Int)] = {
  for (i <- (1 to max).toList; j <- 1 to max
       if isPrime(i + j))
    yield (i, j)
}





def unique(l: List[(Int, Int)]): List[(Int, Int)] = l match {
  case Nil => List.empty[(Int, Int)]
  case (head@(a, b)) :: tail => {
    val res: List[(Int, Int)] = unique(tail)
    if (res.contains((b, a))) res else head :: res
  }
}


def unique2(l: List[(Int, Int)]): List[(Int, Int)] =
  l.foldRight(List.empty[(Int, Int)]) {
    case (head, acc) =>
      if (acc.contains(head.swap)) acc else head :: acc
  }

val primes = primeSum(10)
unique(primes)
unique2(primes)




