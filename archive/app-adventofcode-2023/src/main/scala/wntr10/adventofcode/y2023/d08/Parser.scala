package wntr10.adventofcode.y2023.d08

import com.google.common.base.{Charsets, Preconditions, Splitter}
import com.google.common.io.Files
import com.google.gson.Gson

import java.io.File
import scala.collection.mutable
import scala.jdk.CollectionConverters.ListHasAsScala

object Parser {

  class Alpha(var a: Yankee, var b: Delta)

  class Bravo(var a: Yankee, var b: Charlie)

  class Charlie(var a: Juliett, var b: Juliett)

  type Delta = Array[Bravo]
  type Victor = List[Yankee]
  type Xray = Int
  type Juliett = String
  type Yankee = String

  def alpha(str: String): Alpha = {
    val gson = new Gson()
    val json = record(str, "\n\n", List(yankee, delta))
    val f = new File("input.json")
    Files.asCharSink(f, Charsets.UTF_8).write(json)
    gson.fromJson(json, classOf[Alpha])
  }

  private def bravo(str: String): String = {
    record(str, '=', List(yankee, charlie))
  }

  private def charlie(str: String): String = {
    record(str, ',', List(juliett, juliett))
  }

  private def delta(str: String): String = {
    array(str, '\n', bravo)
  }

  private def juliett(str: String): String = {
    yankee(str
      .replace("(", "")
      .replace(")", "")
      .replace("[", "")
      .replace("]", "")
      .replace("{", "")
      .replace("}", "")
    )
  }

  private def yankee(str: String): String = {
    s"\"$str\""
  }

  private def array(str: String, separator: Char, sub: String => String): String = {
    arrayInt(Splitter.on(separator).omitEmptyStrings().trimResults().splitToList(str).asScala, sub)
  }

  private def arrayInt(buffer: mutable.Buffer[String], sub: String => String): String = {
    buffer.map { p =>
      sub(p)
    }.mkString("[", ", ", "]")
  }

  private def record(str: String, separator: String, sub: List[String => String]): String = {
    val buffer = Splitter.on(separator).omitEmptyStrings().trimResults().splitToList(str).asScala
    recordInt(buffer, sub)
  }

  private def record(str: String, separator: Char, sub: List[String => String]): String = {
    val buffer = Splitter.on(separator).omitEmptyStrings().trimResults().splitToList(str).asScala
    recordInt(buffer, sub)
  }

  private def recordInt(buffer: mutable.Buffer[String], sub: List[String => String]): String = {
    Preconditions.checkArgument(buffer.length == sub.length)
    val keys = buffer.indices.map(i => ('a' + i).toChar.toString)
    buffer.zip(keys).zip(sub).map { p =>
      s"\"${p._1._2}\": ${p._2(p._1._1)}"
    }.mkString("{", ", ", "}")
  }

}
