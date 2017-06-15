import scala.annotation.tailrec

def foo(x: Int) = {
  println("Foo!")
  x + 1
}

def bar(x: => Int) = {
  println("x1 = " + x)
  println("x2 = " + x)
}

bar(foo(3))

// ----------------

val xs = (5 to 10).toList
val ys = (1 to 12 by 2).toList

xs.zip(xs.indices).filter(q => ys.contains(q._1)).map(p => p._2)

for (i <- xs.indices.toList if ys.contains(xs(i))) yield i

// ----------------

implicit def i2s(i: Int): String = i.toString


val l = List(1, 2, 3, 4)
def concat(b: Any, a: Any) = b + "-" + a
l reduceLeft (concat)



// ----------------

def toUpper(s: String) = {
  def up(l: List[Char]): List[Char] = l match {
    case h :: t => h.toUpper :: up(t)
    case _ => Nil
  }

  up(s.toList).mkString
}

// LIRE LES CONSIGNES !!
def toUpperTailRec(s: String) = {
  @tailrec
  def rec(ls: List[Char], acc: String): String = {
    if (ls.isEmpty) acc
    else rec(ls.tail, acc + ls.head.toUpper)
  }

  rec(s.toList, "")
}

toUpper("lala prout")
toUpperTailRec("lala prout")


def balanceMatch(chars: List[Char]): Boolean = {
  def bm(ch: List[Char], acc: List[Char]): Boolean = ch match {
    case h :: t if h == '(' => bm(t, h :: acc)
    case h :: t if h == ')' =>
      if (acc.isEmpty) false
      else bm(t, acc.tail)
    case h :: t => bm(t, acc)
    case Nil => acc.isEmpty
  }

  bm(chars, List.empty[Char])
}

balanceMatch("(if (x == 0) then max (1, x))".toList)
balanceMatch("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList)
balanceMatch("())()".toList)


// --------------

val bs = List(false, true, true, false, true, false, true, true)

bs.foldLeft(0) {
  case (acc, true) => acc + 1
  case (acc, false) => acc
}

// --------------

case class Customer(name: String, id: Int, orders: List[Item])

case class Item(name: String, qty: Int, price: Int)

val o1 = Item("Milk", 1, 1)
val o2 = Item("Pizza", 2, 5)
val o3 = Item("Coke", 1, 3)
val o4 = Item("Beer", 6, 3)

val c1 = Customer("John Doe", 123, List(o1, o2))
val c2 = Customer("Homer Simpson", 124, List(o1, o3, o4))
val c3 = Customer("Marge Simpson", 125, List(o4, o4))

def check(c: Customer): Int =
  c.orders.map(it => it.price * it.qty).sum

assert(check(c1) == 11)
assert(check(c2) == 22)

val merde = Map.empty[String, Int]
val i = 1


def getItems1(ls: List[Customer]) =
  ls.flatMap(_.orders).foldLeft(Map.empty[String, Int]) {
    (acc: Map[String, Int], i: Item) =>
      if (acc.contains(i.name)) acc + (i.name -> (acc(i.name) + 1))
      else acc + (i.name -> 1)
  }


def getItems2(ls: List[Customer]) =
  ls.flatMap(_.orders).foldLeft(Map.empty[String, Int]) {
    (acc: Map[String, Int], i: Item) =>
      acc + (i.name -> (acc.getOrElse(i.name, 0) + 1))
  }

def getItems3(ls: List[Customer]) =
  ls.
    flatMap {_.orders.map(i => (i.name, i.qty))}.
    groupBy(_._1).
    mapValues(_.length)

println(getItems2(List(c1, c2, c3)))
println(getItems3(List(c1, c2, c3)))

// ----------

val primes: Stream[Int] = 2 #:: Stream.from(3).filter { n => !primes.takeWhile(_ <= math.sqrt(n)).exists(n % _ == 0) }

def factors(n: Int): Stream[Int] = {

  def f(n: Int, ps: Stream[Int], res: Stream[Int]): Stream[Int] = {
    if (n <= 1) {
      res
    } else if (n % ps.head == 0) {
      f(n / ps.head, ps, ps.head #:: res)
    } else {
      f(n, ps.tail, res)
    }
  }

  f(n, primes, Stream.Empty)
}

def factors2(n: Int): Stream[Int] = {
  def f(n: Int, ps: Stream[Int], res: List[Int]): List[Int] = {
    if (n <= 1) {
      res
    } else if (n % ps.head == 0) {
      f(n / ps.head, ps, ps.head :: res)
    } else {
      f(n, ps.tail, res)
    }
  }

  f(n, primes, List.empty[Int]).reverse.toStream
}


factors(90).print
factors2(90).print

// ----------

