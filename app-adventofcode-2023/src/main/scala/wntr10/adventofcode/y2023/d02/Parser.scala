package wntr10.adventofcode.y2023.d02

import com.google.common.base.{Charsets, Preconditions}
import com.google.common.io.Files
import com.google.gson.Gson

import java.io.File

object Parser {

  type Alpha = Array[Bravo]

  class Bravo(var a: Int, var b: Array[Delta])

  type Delta = Array[Echo]

  class Echo(var a: Int, var b: String)

  def alpha(a: String): Alpha = {
    val gson = new Gson()
    val json = array(a, '\n', bravo)
    val f = new File("input.json")
    Files.asCharSink(f, Charsets.UTF_8).write(json)
    gson.fromJson(json, classOf[Alpha])
  }

  private def bravo(b: String): String = {
    record(b, ':', List(charlie, delta))
  }

  private def charlie(c: String): String = {
    c.substring("Game ".length)
  }

  private def delta(d: String): String = {
    array(d, ';', echo)
  }

  private def echo(e: String): String = {
    array(e, ',', foxtrot)
  }

  private def foxtrot(f: String): String = {
    record(f, ' ', List(xray, yankee))
  }

  private def xray(x: String): String = x.toInt.toString

  private def yankee(y: String): String = s"\"$y\""

  private def array(str: String, c: Char, sub: String => String): String = {
    str.split(c).map { p =>
      sub(p.trim)
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
