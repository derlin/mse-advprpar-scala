package course9

/**
  * date: 03.05.17
  *
  * @author Lucy Linder <lucy.derlin@gmail.com>
  */
abstract sealed class Temperature(val temp: Double)

object Temperature {
  implicit def DoubleToCelsius(d: Double): Celsius = Celsius(d)

  implicit def DoubleToKelvin(d: Double): Kelvin = Kelvin(d)

  // no need: int <=> double is "built-in"
  // implicit def IntToCelsius(i: Int) : Celsius = Celsius(i)
  // implicit def IntToKelvin(i: Int) : Kelvin = Kelvin(i)

  implicit def KelvinToCelsius(k: Kelvin): Celsius = Celsius(k.temp - 273.15)

  implicit def CelsiusToKelvin(c: Celsius): Kelvin = Kelvin(c.temp + 273.15)
}

case class Celsius(t: Double) extends Temperature(t) {
  require(t >= -273.15)   // don't forget the checks !
  override def toString: String = s"${temp}° C"
}

case class Kelvin(t: Double) extends Temperature(t) {
  require(t >= -459.67)
  override def toString: String = s"${temp}° K"
}

object TempApp extends App {

  import Temperature._

  val a: Celsius = 30   // IntelliJ warning
  val b: Kelvin = 30
  val c: Kelvin = Celsius(10)
  val d: Celsius = c
  val e: Temperature = d

  println("a", a)
  println("b", b)
  println("c", c)
  println("d", d)
  println("e", e)
}