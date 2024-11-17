package wntr10.adventofcode.y2023.d05

import com.google.common.base.{Charsets, Preconditions, Splitter}
import com.google.common.io.Files
import com.google.gson.Gson

import java.io.File
import scala.collection.mutable
import scala.jdk.CollectionConverters.ListHasAsScala

object Parser {

  type Alpha = Array[Bravo]

  class Bravo(var a: Charlie, var b: Delta)

  type Charlie = Yankee
  type Delta = Array[Echo]
  type Echo = Array[Xray]
  type Xray = Long
  type Yankee = String


  def alpha(a: String): Alpha = {
    val gson = new Gson()
    val json = array(a, "\n\n", bravo)
    val f = new File("input.json")
    Files.asCharSink(f, Charsets.UTF_8).write(json)
    gson.fromJson(json, classOf[Alpha])
  }

  private def bravo(b: String): String = {
    record(b, ':', List(charlie, delta))
  }

  private def charlie(c: String): String = {
    yankee(c)
  }

  private def delta(d: String): String = {
    array(d, "\n", echo)
  }

  private def echo(e: String): String = {
    array(e, " ", xray)
  }

  private def xray(x: String): String = {
    x.toLong.toString
  }

  private def yankee(y: String): String = {
    s"\"$y\""
  }

  private def array(str: String, c: String, sub: String => String): String = {
    val list = Splitter.on(c).omitEmptyStrings().trimResults().splitToList(str).asScala
    list.map { p =>
      sub(p)
    }.mkString("[", ", ", "]")
  }

  private def record(str: String, c: Char, sub: List[String => String]): String = {
    val buffer = Splitter.on(c).omitEmptyStrings().trimResults().splitToList(str).asScala
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
