abstract class IntSet() {

  def add(x: Int): IntSet

  def + = this.add(_: Int)

  def excl(x: Int): IntSet

  def - = this.excl(_: Int)

  def contains(x: Int): Boolean

  def foreach(f: Int => Unit): Unit

  def union(that: IntSet): IntSet

  def intersection(that: IntSet): IntSet
}


// object == singleton
object Empty extends IntSet {

  def add(x: Int): IntSet = new NonEmpty(x, Empty, Empty)

  def excl(x: Int): IntSet = this

  def contains(x: Int): Boolean = false

  def foreach(f: Int => Unit): Unit = Unit

  def union(that: IntSet): IntSet = that

  def intersection(that: IntSet): IntSet = this

  override def toString: String = "-"
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet() {

  def add(x: Int): IntSet = {
    if (x < elem) new NonEmpty(elem, left add x, right)
    else if (x > elem) new NonEmpty(elem, left, right add x)
    else this // avoid duplicates
  }

  def excl(x: Int): IntSet = {
    if (x < elem) new NonEmpty(elem, left excl x, right)
    else if (x > elem) new NonEmpty(elem, left, right excl x)
    else left.union(right)
  }

  def contains(x: Int): Boolean = {
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  }

  def foreach(f: Int => Unit): Unit = {
    left.foreach(f)
    f(elem)
    right.foreach(f)
  }

  // why exactly doing union.add.union leads to stackoverflow ?
  def union(that: IntSet): IntSet = that.union(left).union(right).add(elem)

  def intersection(that: IntSet): IntSet =
    if (that.contains(elem)) that.intersection(left) union that.intersection(right).add(elem)
    else that.intersection(left) union that.intersection(right)

  override def toString: String = f"($left|$elem|$right)"

}

Empty
Empty + 3 + 4

def set2 = Empty + 3 + 4 + 2
set2.toString
def set3 = Empty + 3 + 2 + 6 + 1
set3.toString
set3.foreach(println)
set3.foreach(x => println(x + 1))

set3.intersection(set2)

Empty + 3 + 2 + 1 + 4 + 5 + 6 - 3 - 1