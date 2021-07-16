package org.example.UralsPrices

import java.util.Date
import scala.io._

object DataParser {
  def parseDataFromResource(filename: String): Array[PriceByPeriod] =
  {
    implicit val codec: Codec = Codec.apply("Windows-1251")

    val src = Source.fromResource(filename)
    val periods: Array[PriceByPeriod] = parseDataFromArray(src.getLines().toArray)
    src.close()

    periods
  }

  def parseDataFromArray(lines: Array[String]): Array[PriceByPeriod] =
  {
    lines.drop(1).map(line =>
    {
      val fields = line.split(';')
      val price = fields(2).replace(',', '.').toDouble
      new PriceByPeriod(GetDateFromString(fields(0)), GetDateFromString(fields(1)), price)
    })
  }
  
  private def GetDateFromString(date: String): Date = {
      val splitted = date.split('.')
      new Date(splitted(2).toInt + 2000, MonthNameToNumber(splitted(1)), splitted(0).toInt)
  }

  private def MonthNameToNumber(monthName: String) : Int =
  {
    monthName match {
      case "янв" => 0
      case "фев" => 1
      case "мар" => 2
      case "апр" => 3
      case "май" => 4
      case "июн" => 5
      case "июл" => 6
      case "авг" => 7
      case "сен" => 8
      case "окт" => 9
      case "ноя" => 10
      case "дек" => 11
    }
  }
}
