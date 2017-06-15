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

object BitCoinsFuturesStaticApp extends App {

  implicit val formats = DefaultFormats

  val doubleKeys = List("bid", "ask", "last", "total_vol", "24h_avg")
  val tsKey = "timestamp"

  def getAsync(url: String): Future[json.JValue] = Future {
    parse(Source.fromURL(url).mkString)
  }

  val usdReq = getAsync("https://api.bitcoinaverage.com/ticker/USD/")
  val euroReq = getAsync("https://api.bitcoinaverage.com/ticker/EUR/")

  val result = for {
    r1 <- usdReq
    r2 <- euroReq
  } yield (r1, r2)

  result onComplete {
    case Success((usd, euro)) => {
      // print headers
      println(s"timestamps => ")
      println("  euro: " + (euro \ tsKey).extract[String])
      println("  usd: " + (usd \ tsKey).extract[String])
      println()
      
      printf("%10s %10s %10s%n", "what", "euro", "usd")
      println("=================================")

      // print values
      for (k <- doubleKeys)
        printf("%10s %10s %10s%n", k, (euro \ k).extract[Double], (usd \ k).extract[Double])
    }

    case Failure(err) => println("error => " + err.getMessage)
  }

  Await.ready(result, 20 seconds)
  Thread.sleep(100)
  println()

}
