package wntr10.adventofcode.y2023.d00

import com.google.common.base.{CharMatcher, Preconditions}
import com.google.common.collect.Iterators

import scala.jdk.CollectionConverters.IteratorHasAsScala

object Scribble extends App {

  val az = CharMatcher.inRange('a', 'z')
  Preconditions.checkState(az.matchesAllOf("afvsfvs"))

  println(s"Number of Lines ${PartOne.length}")

  val it = Iterators.cycle("AoC": _*).asScala
  println(it.take(7).mkString("[", ",", "]"))

  val aoc = "AoC"
  val aocaoc = Range(0, 7).map { idx =>
    aoc.charAt(idx % aoc.length).toString
  }.mkString("[", ",", "]")
  println(aocaoc)

}
