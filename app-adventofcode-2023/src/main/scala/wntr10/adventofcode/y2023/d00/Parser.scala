package wntr10.adventofcode.y2023.d00

import com.google.common.base.{Charsets, Preconditions}
import com.google.common.io.Files
import com.google.gson.Gson

import java.io.File

object Parser {

  type Alpha = Array[Yankee]

  //type Bravo = String
  //type Charlie = String
  //type Delta = String
  //class Echo(var a: String, var b: String)

  type Victor = List[Yankee]
  type Whiskey = Double
  type Xray = Int
  type Yankee = String
  type Zulu = Boolean

  def alpha(a: String): Alpha = {
    val gson = new Gson()
    val json = array(a, '\n', yankee)
    val f = new File("input.json")
    Files.asCharSink(f, Charsets.UTF_8).write(json)
    gson.fromJson(json, classOf[Alpha])
  }

  // record(b, ':', List(charlie, delta))
  // c.substring("Game ".length)
  // array(d, ';', echo)

  private def bravo(b: String): String = {
    b
  }

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
    array(v, yankee)
  }

  private def whiskey(w: String): String = w.toDouble.toString

  private def xray(x: String): String = x.toInt.toString

  private def yankee(y: String): String = s"\"$y\""

  private def zulu(z: String): String = z.toBoolean.toString

  private def array(str: String, c: Char, sub: String => String): String = {
    str.split(c).map { p =>
      sub(p.trim)
    }.mkString("[", ", ", "]")
  }

  private def array(str: String, sub: String => String): String = {
    str.map { p =>
      sub(p.toString)
    }.mkString("[", ", ", "]")
  }

  private def record(str: String, c: Char, sub: List[String => String]): String = {
    val arr = str.split(c)
    Preconditions.checkArgument(arr.length == sub.length)
    val keys = arr.indices.map(i => ('a' + i).toChar.toString)
    arr.zip(keys).zip(sub).map { p =>
      s"\"${p._1._2}\": ${p._2(p._1._1)}"
    }.mkString("{", ", ", "}")
  }

}
