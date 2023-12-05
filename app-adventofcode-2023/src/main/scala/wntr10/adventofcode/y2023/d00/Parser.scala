package wntr10.adventofcode.y2023.d00

import com.google.common.base.{Charsets, Preconditions, Splitter}
import com.google.common.io.Files
import com.google.gson.Gson

import java.io.File
import scala.collection.mutable
import scala.jdk.CollectionConverters.ListHasAsScala

object Parser {

  type Alpha = Array[Bravo]

  type Bravo = Yankee

  //type Charlie = String
  //type Delta = String
  //class Echo(var a: String, var b: String)

  type Victor = List[Yankee]
  type Whiskey = Double
  type Xray = Int
  type Yankee = String
  type Zulu = Boolean

  def alpha(a: String): Alpha = {
    println(s"alpha of <$a>")
    val gson = new Gson()
    val json = array(a, '\n', bravo)
    val f = new File("input.json")
    Files.asCharSink(f, Charsets.UTF_8).write(json)
    gson.fromJson(json, classOf[Alpha])
  }

  private def bravo(b: String): String = {
    println(s"bravo of <$b>")
    yankee(b)
  }

  // record(b, ':', List(charlie, delta))
  // xray(c.substring("Game".length).trim)
  // array(d, ';', echo)

  private def charlie(c: String): String = {
    c
  }

  private def delta(d: String): String = {
    d
  }

  private def echo(e: String): String = {
    e
  }

  private def foxtrot(f: String): String = {
    f
  }

  private def victor(v: String): String = {
    //println(s"victor of <$v>")
    array(v, yankee)
  }

  private def whiskey(w: String): String = {
    //println(s"whiskey of <$w>")
    w.toDouble.toString
  }

  private def xray(x: String): String = {
    //println(s"xray of <$x>")
    x.toInt.toString
  }

  private def yankee(y: String): String = {
    //println(s"yankee of <$y>")
    s"\"$y\""
  }

  private def zulu(z: String): String = {
    //println(s"zulu of <$z>")
    z.toBoolean.toString
  }

  private def array(str: String, c: Char, sub: String => String): String = {
    val list = Splitter.on(c).omitEmptyStrings().trimResults().splitToList(str).asScala
    list.map { p =>
      sub(p)
    }.mkString("[", ", ", "]")
  }

  private def arrayPreserveEmptyStrings(str: String, c: Char, sub: String => String): String = {
    val list = Splitter.on(c).trimResults().splitToList(str).asScala
    list.map { p =>
      sub(p)
    }.mkString("[", ", ", "]")
  }

  private def array(str: String, sub: String => String): String = {
    str.map { p =>
      sub(p.toString)
    }.mkString("[", ", ", "]")
  }

  private def record(str: String, c: Char, sub: List[String => String]): String = {
    val buffer = Splitter.on(c).omitEmptyStrings().trimResults().splitToList(str).asScala
    recordInt(buffer, sub)
  }

  private def recordPreserveEmptyStrings(str: String, c: Char, sub: List[String => String]): String = {
    val buffer = Splitter.on(c).trimResults().splitToList(str).asScala
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
