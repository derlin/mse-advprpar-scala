package course8

import utils._;

/**
  * date: 26.04.17
  *
  * @author Lucy Linder <lucy.derlin@gmail.com>
  */
object IntegrateApp extends App {

  def integrate(a: Double, b: Double, nIntervals: Long, f: (Double => Double)) = {
    val step = (b - a) / nIntervals
    step / 2 * {f(a) + f(b) + 2 * (a + step until b by step).map(f).sum}
  }

  def f1 = math.sin _

  def a = {
    // (a)

    println(integrate(1, 2, 100000, f1))
  }

  // (b)
  def f2 = math.sin _ compose {math.cos _}

  def b = {

    println(integrate(0, 1, 100000, f2))
  }

  // (c)
  def c = {
    timeVerbose("integrate") {
      println(integrate(0, 1, 64000000, f2))
    }
  }

  // (d)
  def integrateLazy(a: Double, b: Double, nIntervals: Long, f: (Double => Double)) = {
    val step = (b - a) / nIntervals
    step / 2 * {f(a) + f(b) + 2 * (a + step until b by step).view.map(f).sum}
  }

  def d = {
    timeVerbose("integrate") {
      println(integrateLazy(0, 1, 64000000, f2))
    }
  }


  // (e)
  def integrateLazyPar(a: Double, b: Double, nIntervals: Long, f: (Double => Double)) = {
    val step = (b - a) / nIntervals
    // don't put .view before .par !!!!! It will run endlessly...
    step / 2 * {f(a) + f(b) + 2 * (a + step until b by step).par.view.map(f).sum}
  }

  def e = {
    timeVerbose("integrate") {
      println(integrateLazyPar(0, 1, 64000000, f2))
    }
  }
  c
  d
  e
}
