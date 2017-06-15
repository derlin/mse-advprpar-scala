object ExprApp extends App {

  import Expr._

  val expr0 = Sum(Product(Number(2), Number(3)), Number(4))
  println("Expr0: " + show(expr0))
  assert(eval(expr0) == 10)

  val expr1 = Product(Number(4), Number(12))
  println("Expr1: " + show(expr1))
  assert(eval(expr1) == 48)

  val expr2 = Product(Sum(Number(2), Number(3)), Number(4))
  println("Expr2: " + show(expr2))
  assert(eval(expr2) == 20)

  val expr3 = Product(Number(2), Sum(Number(3), Number(4)))
  println("Expr3: " + show(expr3))
  assert(eval(expr3) == 14)
}


sealed abstract class Expr

case class Number(n: Int) extends Expr

case class Sum(e1: Expr, e2: Expr) extends Expr

case class Product(e1: Expr, e2: Expr) extends Expr

object Expr {
  def eval(e: Expr): Int = e match {
    case Number(n) => n
    case Sum(e1, e2) => eval(e1) + eval(e2)
    case Product(e1, e2) => eval(e1) * eval(e2)
  }

  def showSimple(e: Expr): String = e match {
    case Number(n) => n.toString
    case Sum(e1, e2) => s"${showSimple(e1)} + ${showSimple(e2)}"
    case Product(e1, e2) => s"${showSimple(e1)} * ${showSimple(e2)}"
  }

  def show(e: Expr): String = e match {
    case Number(n) => n.toString
    case Sum(e1, e2) => s"${show(e1)} + ${show(e2)}"
    case Product(e1@Sum(_, _), e2@Sum(_, _)) => s"(${show(e1)}) * ${show(e2)}"
    case Product(e1@Sum(_, _), e2) => s"(${show(e1)}) * ${show(e2)}"
    case Product(e1, e2@Sum(_, _)) => s"${show(e1)} * (${show(e2)})"
    case Product(e1, e2) => s"${show(e1)} * ${show(e2)}"
  }
}


