package wntr10.adventofcode.y2023.d15

import wntr10.adventofcode.Input


object PartTwo extends App {

  private lazy val input: Parser.Alpha = {
    val input = new Input(this.getClass.getName)
    val lines = input.read

    Parser.parse(lines)
  }

  lazy val length: Int = input.length

  private case class Lens(label: String, fl: Int)

  private case class HASHMAP(hash: Int, label: String, focalLength: Option[Int]) {
    def lensOpt: Option[Lens] = focalLength.map(Lens(label, _))
  }

  private val boxes = Array.tabulate(256)(_ => List.empty[Lens])

  private def hashmap(str: String): HASHMAP = {
    val e = str.indexOf('=')
    val d = str.indexOf('-')
    val opIdx = Math.max(e, d)
    val s = str.substring(0, opIdx)
    val op = str.charAt(opIdx)
    val fl = if (op == '=') {
      Some(str.substring(opIdx + 1).toInt)
    } else {
      None
    }

    var i = 0
    var value = 0
    while (i < s.length) {
      val char = s.charAt(i)
      val ascii = char.toByte.toInt
      value += ascii
      value = value * 17
      value = value % 256

      i += 1
    }

    HASHMAP(value, s, fl)
  }

  private def solve(input: Parser.Alpha): Int = {
    input.foreach { line =>
      line.foreach { step =>
        val instr = hashmap(step)

        val lenses = boxes(instr.hash)
        boxes(instr.hash) = instr.lensOpt match {
          case Some(lens) =>
            val i = lenses.indexWhere(l => l.label == instr.label)
            if (i != -1) {
              lenses.updated(i, lens)
            } else {
              lens :: lenses
            }
          case _ =>
            lenses.filter(l => l.label != instr.label)
        }
      }
    }

    boxes.zipWithIndex.filter(t => t._1.nonEmpty).map { bz =>
      bz._1.reverse.zipWithIndex.map { lz =>
        (bz._2 + 1) * (lz._2 + 1) * lz._1.fl
      }.sum
    }.sum

  }

  println(solve(input))

}
