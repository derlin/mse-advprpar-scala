// ## append, apply, collect

// `++` or `++:` : append two lists
List(1, 2) ++: List(3, 4) // List(1, 2, 3, 4)
// `:+` prepend, `+:` append
3 +: List(1, 2) // List(3, 1, 2)
List(1, 2) :+ 3 // List(1, 2, 3)
// `::` prepend (right associative) => 3 becomes the head
3 :: List(1, 2) // List(3, 1, 2)
List(1, 2).::(3) // List(3, 1, 2)

// apply : selects and element at the given index
List.range(0, 3).apply(1) // 1

// collect: apply a partial function, keep only the matches
List(1, 'a', "str").collect {
  case x: Int => x
  case c: Char => c.toInt
} // List(1, 97)

// collectFirst: returns Some(first match) or None
List('x', 'a', "str").collectFirst {
  case x: Int => x
} // None

// corresponds: compare two lists element-wise
List(1,2,3).corresponds(List(11,12,13)){
  case (a, b) => a + 10 == b
}  // true

// count: same as filter.length
List(1,2,3).count(_ == 2)
List(1,2,3).filter(_ == 2).length

// groupBy: returns a Map
List(('a', 1), ('b', 2), ('a',2)).groupBy(_._1)
//> Map(b -> List((b,2)), a -> List((a,1), (a,2)))

// grouped: returns an iterable
List.range(0, 10).grouped(3).toList
//> List(List(0, 1, 2), List(3, 4, 5), List(6, 7, 8), List(9))

// intersect: keeps duplicates, if any
List(1,1,1,2,4).intersect(List(1,1,2,3))
//> List(1, 1, 2)

// maxBy: max depending on the result of fn
List(2,10).maxBy(_ % 10) //> 2

// partition
List(1,2,3,4).partition( _ % 2 == 0)
//> (List(2, 4),List(1, 3))

// patch: insert a sequence at index <from>, replacing <replaced> elements. if <replaced> = 0, it inserts the sequence
List(1,4).patch(from = 1, List(2,3), replaced = 0)
//> List(1, 2, 3, 4)
List(1,4,1).patch(1, List(2,3), 2)
//> List(1, 2, 3)

// sliding: sliding window
List(1,2,3,4).sliding(2).toList
//> List(List(1, 2), List(2, 3), List(3, 4))

// ------------------

// combination: return all the possible combinations (order does not count) as a Seq
List(1, 2, 3).combinations(2).toList
//> List(List(1, 2), List(1, 3), List(2, 3))
"hello".combinations(3).toList
//> List(hel, heo, hll, hlo, ell, elo, llo)
def allCombis(ls: List[Any]) = (1 to ls.length).flatMap(ls.combinations(_).toList)
// List(1,2,3) =>  Vector(List(1), List(2), List(3), List(1, 2), List(1, 3), List(2, 3), List(1, 2, 3))

// permutation: return all the possible permutations (order does count) as a Seq
List(1, 2).permutations.toList
//> List(List(1, 2), List(2, 1))


