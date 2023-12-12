package wntr10.adventofcode.y2023.d00

import com.google.common.base.{CharMatcher, Preconditions}
import com.google.common.collect.Iterators
import org.roaringbitmap.RoaringBitmap

import scala.collection.mutable
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

  val bs = new mutable.BitSet()

  bs.add(4)
  bs.add(6)
  bs.add(13)

  println(bs)
  println(bs.size)
  println(bs.head)

  val rr = RoaringBitmap.bitmapOf(7,8,10,15)
  println(rr)
  val r2 = RoaringBitmap.addOffset(rr, - 7)
  println(r2)

}
