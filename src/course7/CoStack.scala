package course7

/**
  * date: 26.04.17
  *
  * @author Lucy Linder <lucy.derlin@gmail.com>
  */
trait Stack[+A] {
  def push[B >: A](elem: B): Stack[B]

  def top: A

  def pop: Stack[A]
}


case object EmptyStackObject extends Stack[Nothing] {
  override def push[B >: Nothing](elem: B) = ElemStack(elem, this)

  override def top: Nothing = sys.error("Trying to top an empty stack")

  override def pop: Stack[Nothing] = this
}

case class EmptyStack[+A]() extends Stack[A] {
  override def push[B >: A](elem: B) = ElemStack(elem, this)

  override def top: A = sys.error("Trying to top an empty stack")

  override def pop: Stack[A] = this
}

case class ElemStack[+A](elem: A, stack: Stack[A]) extends Stack[A] {

  override def push[B >: A](elem: B) = ElemStack(elem, this)

  override def top: A = elem

  override def pop: Stack[A] = stack

  override def toString: String = s"$elem,$stack"
}


object StackApp extends App {
  // Construction, pop and toString
  val a = EmptyStack().push("hello").push("world").push("it’s fun").pop

  assert(a.toString() == "world,hello,EmptyStack()")

  // Getting top
  val b = EmptyStack().push(1).push(3)
  assert(b.top == 3)

  // Variance checks
  class Foo

  class Bar extends Foo

  val c: Stack[Bar] = EmptyStack().push(new Bar()).push(new Bar())
  assert(c.top.isInstanceOf[Bar] == true)
  assert(c.top.isInstanceOf[Foo] == true)

  // Variance check 2
  val d: Stack[Foo] = EmptyStack().push(new Bar()).push(new Bar())
  assert(d.top.isInstanceOf[Foo])
}
