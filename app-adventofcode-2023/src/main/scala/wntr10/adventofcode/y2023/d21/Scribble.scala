package wntr10.adventofcode.y2023.d21

object Scribble extends App {

  Table.read()
  Table.readTiles()

  val e = Table.table.filter(f => f._1._1 == 'E')
  val a = Table.table.filter(f => f._1._1 == 'A').map(u => u._1._2 -> u._2)
  val ev = e.values.toSet
  val sp = e.flatMap { u =>
    Set(u._1._2, u._1._3, u._1._4, u._1._5, u._1._6)
  }.toSet

  val g = ev.map { v =>
    var l = List.empty[Int]
    var c = v

    while (!l.contains(c)) {
      l = c :: l
      c = a(c)
    }
    l.reverse
  }

  g.foreach { l =>
    println(l)
    println(" " + l.zipWithIndex.filter(z => sp.contains(z._1)))
    val ll = l.size - 1
    val lll = l.size - 2
    println("@" + ll + "=" + Table.tiles(l(ll)))
    println("@" + lll + "=" + Table.tiles(l(lll)))
    println("@65=" + Table.tiles(l(65)))
    println("@130=" + Table.tiles(l(130)))
    println("@64=" + Table.tiles(l(64)))
    println("@195=" + Table.tiles(l(195)))
  }

  val vs = Table.table.values.toSet
  val st = a.filter(u => !vs.contains(u._1))
  println(st)


  var l = List.empty[Int]
  var c = st.head._1

  while (!l.contains(c)) {
    l = c :: l
    c = a(c)
  }

  l = l.reverse
  println(l)

  println(" " + l.zipWithIndex.filter(z => sp.contains(z._1)))
  println("@64=" + Table.tiles(l(64)))
  println("@65=" + Table.tiles(l(65)))
  println("size =" + l.size)
  val ll = l.size - 1
  val lll = l.size - 2

  println("@" + ll + "=" + Table.tiles(l(ll)))
  println("@" + lll + "=" + Table.tiles(l(lll)))

  val number = 202300

  var sum = BigInt(0)

  Range(1, number).foreach { f =>
    if (f % 2 == 0) {
      sum = sum + (7362 * 4)
    } else {
      sum = sum + (7354 * 4)
    }
  }

  Range(1, number - 1).foreach { g =>
    Range.inclusive(1, g).foreach { f =>
      if (f % 2 == 0) {
        sum = sum + (7362 * 4)
      } else {
        sum = sum + (7354 * 4)
      }
    }
  }

  Range(0, number).foreach { f =>
    sum = sum + 925
    sum = sum + 937
    sum = sum + 941
    sum = sum + 936
  }

  Range(1, number).foreach { f =>
    sum = sum + 6461
    sum = sum + 6456
    sum = sum + 6444
    sum = sum + 6459
  }

  sum = sum + 5538
  sum = sum + 5541
  sum = sum + 5555
  sum = sum + 5558

  if (number % 2 == 0) {
    sum = sum + 7362
  } else {
    sum = sum + 7354
  }

  println("number = " + number)
  println("step = " + (BigInt(65) + (number * 131)))
  println("sum = " + sum)

}
