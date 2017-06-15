// ## Higher-order functions on lists

// Using only the zip and map higher-order functions, define the following functions that:
// (a) Returns a list of the length of each string of a list

def lengthStrings = (_: List[String]).map(_.length)

lengthStrings(List("How", "long", "are", "we?")) == List(3, 4, 3, 3)

// (b) Produces a list with n identical elements of arbitrary type (don’t use the fill method!)

def dup(elem: Any, n: Int) = (0 until n).map(_ => elem).toList
dup("foo", 5) ==
  List("foo", "foo", "foo", "foo", "foo").asInstanceOf[List[Any]]
dup(List(1, 2, 3), 2) ==
  List(List(1, 2, 3), List(1, 2, 3)).asInstanceOf[List[Any]]

// (c) Multiplies element-wise two lists of values and create a new list

def dot(l1: List[Int], l2: List[Int]) =
  (l1 zip l2).map(t => t._1 * t._2)

dot(List(1, 2, 3), List(2, 4, 3)) == List(2, 8, 9)

// ## Folding functions on lists
// Now using folding (right or left) define a function that:

// (a) Determines if all logical values in a non-empty list are true.

// Note: here, foldRight would work as well.
def areTrue(list: List[Boolean]) = list.foldLeft(true) {
  (acc, elt) => elt && acc
}

areTrue(List(true, true, false)) == false
areTrue(List(true, true, true)) == true

// The function `forall` does the same:
List(true, true, false).forall(b => b) == false
List(true, true, true).forall(b => b) == true

// (b) Determine the total length of the strings in a list Example:

def lString(list: List[String]) = list.foldLeft(0) {
  (acc, elt) => acc + elt.length
}

lString(List("Folding", "is", "fun")) == 12


// (c) Returns the longest string of a list as well as its size Example:

def longest(list: List[String]) = list.foldLeft((0, "")) {
  (acc, elt) => if (acc._1 < elt.length) (elt.length, elt) else acc
}

longest(List("What", "is", "the", "longest?")) == (8, "longest?")

// (d) Decide if a value is an element of a list of an arbitrary type

def isPresent(list: List[Any], value: Any) = list.foldLeft(false) {
  (acc, elt) => acc || (elt == value)
}

isPresent(List(1, 2, 3, 4), 5) == false
isPresent(List(1, 2, 3, 4), 3) == true

// (e) Flatten a nested list structure of any type.

def flattenList(list: List[Any]): List[Any] = list.foldRight(List.empty[Any]) {
  (elt, acc) =>
    elt match {
      case l: List[Any] => flattenList(l) ++ acc
      case _ => elt :: acc
    }
}

flattenList(List(List(1, 1), 2, List(3, List(5, 8)))) ==
  List(1, 1, 2, 3, 5, 8)