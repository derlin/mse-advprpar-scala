/**
  * date: 15.03.17
  *
  * @author Lucy Linder <lucy.derlin@gmail.com>
  */


object BinaryTreeApp extends App {

  def tree = Node(Node(Leaf(4), Leaf(1)), Node(Node(Leaf(4), Leaf(6)), Leaf(5)))

  println(tree)
  println(tree.min())
  println(tree.sum())
}

sealed abstract class BinaryTree {
  def sum(): Int = this match {
    case Leaf(v) => v
    case Node(left, right) => left.sum + right.sum
  }

  def min(): Int = {
    def _min(t: BinaryTree, acc: Int): Int = t match {
      case Leaf(v) => if (v < acc) v else acc
      case Node(left, right) => {
        val l = _min(left, acc)
        val r = _min(right, acc)
        if (l < r) l else r
      }
    }

    _min(this, Int.MaxValue)
  }
}

case class Leaf(value: Int) extends BinaryTree

case class Node(left: BinaryTree, right: BinaryTree) extends BinaryTree

