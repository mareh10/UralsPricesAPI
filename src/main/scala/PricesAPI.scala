package org.example.UralsPrices

import java.util.Date
import io.circe._, io.circe.parser._


object PricesAPI {

  def priceAtDate(date: Date): Double = {
    CachedData.getCachedData.find(period => date.after(period.periodBegin) && date.before(period.periodEnd)) match {
      case Some(value) => value.price
      case None => 0
    }
  }

  def averagePriceAtPeriod(periodBegin: Date, periodEnd: Date): Double = {
    val periods = GetPeriodsByDates(CachedData.getCachedData ,periodBegin, periodEnd)

    periods.map(period => period.price).sum / periods.length
  }

  def maxAndMinPricesAtPeriod(periodBegin: Date, periodEnd: Date): Json = {
    val periods = GetPeriodsByDates(CachedData.getCachedData ,periodBegin, periodEnd)

    val maxByPeriod = periods.maxByOption(period => period.price) match {
      case Some(value) => value.price
      case None => 0
    }

    val minByPeriod = periods.minByOption(period => period.price) match {
      case Some(value) => value.price
      case None => 0
    }

    parse(s"""
    {
        "min": ${minByPeriod},
        "max": ${maxByPeriod}
    }
    """).getOrElse()
  }

  def allCollectedData(): JsObject = { //JSON
    implicit val periodWrites = new Writes[PriceByPeriod] {
      def writes(priceByPeriod: PriceByPeriod) =
        JsObject(
          Seq(
            "periodBegin" -> JsString(priceByPeriod.periodBegin.toString),
            "periodEnd" -> JsString(priceByPeriod.periodEnd.toString),
            "price" -> JsNumber(priceByPeriod.price)
          )
        )
    }

    JsObject(
      Seq(
        "records" -> JsArray(
          CachedData.getCachedData.map(period => Json.toJson(period))
        )
      )
    )
  }

  private def GetPeriodsByDates(periods: Array[PriceByPeriod], periodBegin: Date, periodEnd: Date) ={
    periods.filter(period => periodBegin.after(period.periodBegin) && periodEnd.before(period.periodEnd) ||
      periodBegin.after(period.periodBegin) && periodBegin.before(period.periodEnd) ||
      periodEnd.after(period.periodBegin) && periodEnd.before(period.periodEnd)) //неочевидно и непонятно
  }
}
