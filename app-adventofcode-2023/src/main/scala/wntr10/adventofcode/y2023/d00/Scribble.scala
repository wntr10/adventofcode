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

  val str = "...####....###...##.."

  val arr = str.toCharArray
  arr(1) = '#'
  val strPrime = arr.mkString

  println(s"<$str> vs <$strPrime>")

  def strip(str: String): String = {
    val sb = new StringBuilder
    sb.append('.')
    var i = 0
    while (i != -1 && i < str.length) {
      val ni = str.indexOf('.', i)
      i = if (ni == -1) {
        sb.append(str.substring(i))
        sb.append('.')
        ni
      } else if (i == ni) {
        str.indexOf('#', ni + 1)
      } else {
        sb.append(str.substring(i, ni + 1))
        str.indexOf('#', ni + 1)
      }
    }
    sb.toString()
  }

  val s0 = strip(str)
  println(s"<$s0>")

  val s1 = strip("###...######...")
  println(s"<$s1> ($s1)")

  val s2 = strip("###.##..#..######")
  println(s"<$s2> ($s2)")

  val s3 = strip("")
  println(s"<$s3> ($s3)")

  val s4 = strip(".")
  println(s"<$s4> ($s4)")

  val s5 = strip("..")
  println(s"<$s5> ($s5)")

  val s6 = strip("#")
  println(s"<$s6> ($s6)")


}
