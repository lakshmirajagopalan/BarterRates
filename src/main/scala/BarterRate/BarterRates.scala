package BarterRate

import scala.collection.immutable.Iterable

/**
 * Created by lakshmi on 5/8/14.
 */
//

class BarterRates(fruitsMap:Map[String, List[(String, Rational)]], targetMap:Map[String, String], visited:scala.collection.mutable.Map[String, Boolean]) {

  def getCost(src: String, dest: String, rate: Rational): Tuple2[Boolean, Rational] = {
    if (src == dest)
      (true, rate)
    else {
      val neighboursCostPair = fruitsMap.getOrElse(src, List[(String, Rational)]()).filter(fruitCost => visited(fruitCost._1) == false)
      println(neighboursCostPair.size)
      var res = (false, rate)
      for (neighbour <- neighboursCostPair) {
        if (res._1 == true)
          return res
        else {
          visited.update(neighbour._1, true)
          println(visited(neighbour._1))
          res = getCost(neighbour._1, dest, new Rational(rate.numer * neighbour._2.numer, rate.denom * neighbour._2.denom))
        }
      }
      res
    }

  }

  def resetVisited = visited.keys.foreach(k=> visited(k) = false)

  def getRate() = {
    val output: Iterable[String] = targetMap
      .map{t =>
        resetVisited
        (t, getCost(t._1, t._2, new Rational(1, 1)))
      }
      .map { (res: ((String, String), (Boolean, Rational))) =>
        val ((key, value), (success, cost)) = res
        "%d%s equals %d%s".format(cost.denom, key, cost.numer, value)
      }.toList
    output
  }
}
object BarterRates {

  def apply(question:String)(input:String):BarterRates = {

    val qList = question.split(",").map(q => q.split(" "))
    val questionMap: Map[String, String] = qList.map(q => (q(0) -> q(1))).toMap

    for(q <- questionMap.keys) println(q, questionMap(q))

    val ratesArr = input.split(",").map(c => c.split(" ")).toList
    val fruitMap = ratesArr.flatMap{fruitMapping =>
      List((fruitMapping(1), (fruitMapping(3), new Rational(fruitMapping(2).toInt, fruitMapping(0).toInt))),
        (fruitMapping(3), (fruitMapping(1), new Rational(fruitMapping(0).toInt, fruitMapping(2).toInt))))
    }

    val fruitAdjacency = fruitMap.groupBy(_._1).map(k => (k._1, k._2.map(t => t._2)))
    val visited = collection.mutable.Map((fruitAdjacency.keys.map(x => (x, false)).toMap).toSeq:_*)
    new BarterRates(fruitAdjacency, questionMap, visited)
  }
}

