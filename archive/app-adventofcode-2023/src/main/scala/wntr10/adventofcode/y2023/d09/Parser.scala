package wntr10.adventofcode.y2023.d09

import com.google.common.base.{Charsets, Splitter}
import com.google.common.io.Files
import com.google.gson.Gson

import java.io.File
import scala.collection.mutable
import scala.jdk.CollectionConverters.ListHasAsScala

object Parser {

  type Alpha = Array[Bravo]

  type Bravo = Array[Xray]

  type Xray = Int

  def alpha(str: String): Alpha = {
    val gson = new Gson()
    val json = array(str, '\n', bravo)
    val f = new File("input.json")
    Files.asCharSink(f, Charsets.UTF_8).write(json)
    gson.fromJson(json, classOf[Alpha])
  }

  private def bravo(str: String): String = {
    array(str, ' ', xray)
  }

  private def xray(str: String): String = {
    str.toInt.toString
  }

  private def array(str: String, separator: Char, sub: String => String): String = {
    arrayInt(Splitter.on(separator).omitEmptyStrings().trimResults().splitToList(str).asScala, sub)
  }

  private def arrayInt(buffer: mutable.Buffer[String], sub: String => String): String = {
    buffer.map { p =>
      sub(p)
    }.mkString("[", ", ", "]")
  }

}
