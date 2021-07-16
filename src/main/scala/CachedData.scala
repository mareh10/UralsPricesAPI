package org.example.UralsPrices

import scala.collection.immutable

object CachedData {
  private implicit val periodOrdering: Ordering[PriceByPeriod] = (x: PriceByPeriod, y: PriceByPeriod) => {
    if (x.periodBegin.before(y.periodBegin)) -1
    else if (x.periodBegin.after(y.periodBegin)) 1
    else 0
  }

  private var data: Array[PriceByPeriod] = Array.empty // var not good

  def getCachedData: Array[PriceByPeriod] = {
    if (data.isEmpty){
      data = DataParser.parseDataFromArray(WebRetriever.retrieveArray(
        "https://data.gov.ru/sites/default/files/otkrytye_dannye_-_cena_na_neft_17.csv")).sorted
    }
    data
  }
}