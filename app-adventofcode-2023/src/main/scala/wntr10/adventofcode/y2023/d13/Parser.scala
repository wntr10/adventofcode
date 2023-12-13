package wntr10.adventofcode.y2023.d13

import com.google.common.base.{CharMatcher, Charsets, Preconditions, Splitter}
import com.google.common.io.Files
import com.google.gson.Gson

import java.io.File
import scala.collection.mutable
import scala.jdk.CollectionConverters.ListHasAsScala

object Parser {

  type Alpha = Array[Bravo]

  type Bravo = Array[Yankee]

  type Victor = List[Yankee]
  type Xray = Int
  type Yankee = String

  def parse(str: String): Alpha = {
    val gson = new Gson()
    val json = alpha(str)
    val f = new File("input.json")
    Files.asCharSink(f, Charsets.UTF_8).write(json)
    gson.fromJson(json, classOf[Alpha])
  }

  def alpha(str: String): String = {
    array(str, "\n\n", charlie)
  }

  private def bravo(str: String): String = {
    yankee(str)
  }

  private def charlie(str: String): String = {
    array(str, "\n", bravo)
  }

  private def yankee(str: String): String = {
    s"\"$str\""
  }

  //alpha                  Array[Bravo]
  //bravo
  //charlie
  //delta
  //echo
  //foxtrot
  //golf
  //hotel
  //india
  //--------
  //juliett  strip ()[]{}  String
  //kilo
  //lima
  //mike
  //november
  //oscar
  //papa
  //quebec
  //--------
  //romeo
  //sierra
  //tango   1212121242     BigInt
  //uniform 1212121242.0   BigDecimal
  //victor  ["a", "y"]     Array of Strings
  //whiskey -6.0, 0.0, 8.0 Double
  //xray    -2, 0, 5       Int
  //yankee  "xyz"          String
  //zulu    true, false    Boolean


  private def array(str: String, separator: Char, sub: String => String): String = {
    arrayInt(Splitter.on(separator).omitEmptyStrings().trimResults().splitToList(str).asScala, sub)
  }

  private def array(str: String, separator: String, sub: String => String): String = {
    arrayInt(Splitter.on(separator).omitEmptyStrings().trimResults().splitToList(str).asScala, sub)
  }

  private def array(str: String, separator: CharMatcher, sub: String => String): String = {
    arrayInt(Splitter.on(separator).omitEmptyStrings().trimResults().splitToList(str).asScala, sub)
  }

  private def array(str: String, sub: String => String): String = {
    arrayInt(str.map(_.toString).toBuffer, sub)
  }

  private def arrayPreserveEmptyStrings(str: String, c: Char, sub: String => String): String = {
    arrayInt(Splitter.on(c).trimResults().splitToList(str).asScala, sub)
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

  private def record(str: String, separator: CharMatcher, sub: List[String => String]): String = {
    val buffer = Splitter.on(separator).omitEmptyStrings().trimResults().splitToList(str).asScala
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
