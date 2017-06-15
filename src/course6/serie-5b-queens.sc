def queens(n: Int): List[List[(Int, Int)]] = {
  def placeQueens(k: Int): List[List[(Int, Int)]] =
    if (k == 0) List(List())
    else for {
      queens <- placeQueens(k - 1)
      column <- 1 to n
      queen = (k, column)
      if isSafe(queen, queens)
    }
      yield queen :: queens

  placeQueens(n)
}

def isSafe(queen: (Int, Int), queens: List[(Int, Int)]) = queens forall (q => !inCheck(queen, q))

def inCheck(q1: (Int, Int), q2: (Int, Int)) =
  q1._1 == q2._1 || // same row
    q1._2 == q2._2 || // same column
    (q1._1 - q2._1).abs == (q1._2 - q2._2).abs // on diagonal

// --------------------
val QUEEN_UNICODE = "\u265b"
val soluce = queens(4)

soluce(0).sorted


def printChessboard1(soluce: List[List[(Int, Int)]]): Unit = {
  val k = soluce(0).length
  for ((s, i) <- soluce.zipWithIndex) {
    println(s"\nSolution ${i+1}")
    for (q <- s.sorted) {
      for (c <- 1 to k) {
        if (c == q._2) print(s"|$QUEEN_UNICODE") else print("|_")
      }
      println("|")
    }
  }
}


def printChessboard2(soluce: List[List[(Int, Int)]]): Unit = {
  val k = soluce(0).length
  for ((s, i) <- soluce.zipWithIndex) {
    // create "cells"
    val str = {
      for (q <- s.sorted; c <- 1 to k)
        yield {if (c == q._2) QUEEN_UNICODE else "_"}
    }

    // print cells
    println(s"\nsolution ${i+1}")
    str.grouped(k).foreach{
      l => println("|" + l.mkString("|") + "|")
    }
  }
}


printChessboard1(soluce)