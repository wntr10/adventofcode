package wntr10.adventofcode.y2023.d03

import com.google.common.base.Charsets
import com.google.common.io.Files
import com.google.gson.Gson

import java.io.File

object Parser {

  type Alpha = Array[Victor]
  type Victor = Array[Yankee]
  type Yankee = String

  def alpha(a: String): Alpha = {
    val gson = new Gson()
    val json = array(a, '\n', victor)
    val f = new File("input.json")
    Files.asCharSink(f, Charsets.UTF_8).write(json)
    gson.fromJson(json, classOf[Alpha])
  }

  private def victor(v: String): String = {
    array(v, yankee)
  }

  private def yankee(y: String): String = s"\"$y\""

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

}
