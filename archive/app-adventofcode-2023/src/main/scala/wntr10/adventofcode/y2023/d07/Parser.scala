package wntr10.adventofcode.y2023.d07

import com.google.common.base.{Charsets, Preconditions, Splitter}
import com.google.common.io.Files
import com.google.gson.Gson

import java.io.File
import scala.collection.mutable
import scala.jdk.CollectionConverters.ListHasAsScala

object Parser {

  type Alpha = Array[Bravo]

  class Bravo(var a: Yankee, var b: Xray)

  type Xray = Int
  type Yankee = String

  def alpha(str: String): Alpha = {
    val gson = new Gson()
    val json = array(str, '\n', bravo)
    val f = new File("input.json")
    Files.asCharSink(f, Charsets.UTF_8).write(json)
    gson.fromJson(json, classOf[Alpha])
  }

  private def bravo(str: String): String = {
    record(str, ' ', List(yankee, xray))
  }

  private def xray(str: String): String = {
    //println(s"xray of <$str>")
    str.toInt.toString
  }

  private def yankee(str: String): String = {
    //println(s"yankee of <$str>")
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
