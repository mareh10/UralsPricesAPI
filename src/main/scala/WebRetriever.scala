package org.example.UralsPrices

import scala.io.{Codec, Source}

object WebRetriever {
  def retrieveArray(url: String) : Array[String] = {
    implicit val codec: Codec = Codec.apply("Windows-1251")
    val src = scala.io.Source.fromURL(url)
    val lines = src.getLines().toArray

    src.close()
    lines
  }
}
