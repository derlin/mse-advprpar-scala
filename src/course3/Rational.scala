package course3

/**
  * date: 08.03.17
  *
  * @author Lucy Linder <lucy.derlin@gmail.com>
  */

object Course3_Rational extends App {
  val r1 = new Rational(2, 3)
  val r2 = new Rational(4, 3)

  val r3 = r1 + r2
  println(r1 max r2)
  println(-r3)
}

class Rational(n: Int, d: Int) {
  require(d != 0)

  // auxiliary constructor
  def this(n: Int) = this(n, 1)

  // specify return type for recursive function
  private def gcd(x: Int, y: Int): Int = if (y == 0) x else gcd(y, x % y)

  private val g = gcd(n, d)

  def num = n / g

  def denom = d / g


  def +(that: Rational) = {
    new Rational(num * that.denom + that.num * denom, denom * that.denom)
  }

  def -(that: Rational) = {
    new Rational(num * that.denom - that.num * denom, denom * that.denom)
  }

  def *(that: Rational) = {
    new Rational(num * that.denom + that.num * denom, denom * that.denom)
  }

  def /(that: Rational) = {
    new Rational(num * that.denom, denom * that.num)
  }

  def ==(that: Rational) = {
    num * that.denom == denom * that.num
  }

  def unary_-() = {
    new Rational(-num, denom)
  }

  def <(that: Rational) = {
    num * that.denom < denom * that.num
  }

  def max(that: Rational) = {
    if (this < that) that else this
  }


  override def toString() = {
    num + "/" + denom
  }

}