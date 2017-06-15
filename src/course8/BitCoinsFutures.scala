package course8

import net.liftweb.json

import scala.concurrent.{Await, Future}
import scala.io.Source
import net.liftweb.json.DefaultFormats
import net.liftweb.json.parse

import scala.collection.immutable.HashMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Failure, Success}

/**
  * date: 26.04.17
  *
  * @author Lucy Linder <lucy.derlin@gmail.com>
  */

object BitCoinsFuturesApp extends App {

  implicit val formats = DefaultFormats


  def getAsync(url: String): Future[json.JValue] = Future {
    parse(Source.fromURL(url).mkString)
  }

  val usdReq = getAsync("https://api.bitcoinaverage.com/ticker/USD/")
  val euroReq = getAsync("https://api.bitcoinaverage.com/ticker/EUR/")

  val result = for {
    r1 <- usdReq
    r2 <- euroReq
  } yield (r1, r2)

  //  result onComplete {
  //    case Success((usd, euro)) => {
  //      printf("%10s %10s %10s%n", "what", "euro", "usd")
  //      println("=================================")
  //
  //      for (f <- euro.values.asInstanceOf[Map[String, _]]
  //           if f._1 != "timestamp") {
  //        var usdVal = (usd \ f._1.toString).values
  //        printf("%10s %10s %10s%n", f._1, f._2, usdVal)
  //      }
  //
  //    }
  //    case Failure(err) => println("error => " + err.getMessage)
  //  }

  result onComplete {
    case Success((usd, euro)) => {

      val keys = euro.values.asInstanceOf[Map[String, _]].keys
      val list = for (key <- keys) yield (key, ((euro \ key).values, (usd \ key).values))

      printResults(list.toMap)

    }
    case Failure(err) => println("error => " + err.getMessage)
  }

  Await.ready(result, 20 seconds)
  Thread.sleep(100)
  println()


  def printResults(map: Map[String, Tuple2[_, _]]) = {
    val ts = map("timestamp")
    println(s"timestamps => euro: ${ts._1} | usd: ${ts._2}")
    printf("%10s %10s %10s%n", "what", "euro", "usd")
    println("=================================")
    for ((k, v) <- map - "timestamp") {
      v match { case (e, u) => printf("%10s %10s %10s%n", k, e, u) }
    }
  }
}
