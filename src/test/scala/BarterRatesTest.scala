/**
 * Created by lakshmi on 5/8/14.
 */
package BarterRate


import org.scalatest.{ShouldMatchers, FlatSpec}

import scala.collection.immutable.Iterable

class BarterRatesTest extends FlatSpec with ShouldMatchers{
  "BarterRates Companion Object" should "return a barter value for one test case" in {
    val barterRate:BarterRates = BarterRates("Grape Apple")("1 Apple 2 Banana,1 Banana 2 Grape")
    barterRate.getRate().head should equal ("4Grape equals 1Apple")
  }

  "BarterRates" should "return barter value for apple-grape trade" in {
    val barterRate:BarterRates = BarterRates("Apple Grape,Grape Apple")("1 Apple 2 Banana,1 Banana 2 Grape")
    val results = barterRate.getRate().toList
    results(0) should equal ("1Apple equals 4Grape")
    results(1) should equal ("4Grape equals 1Apple")

  }


}
