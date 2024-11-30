import $ivy.`org.jgrapht:jgrapht-core:1.5.2`
import $ivy.`com.google.guava:guava:33.3.1-jre`
import $file.^.Basic, Basic._, Input._

val ex = ".ex0" // 291
val input = read(s"day22$ex")
val parts = split("Player ", input)
var players = Vector.empty[Vector[Int]]
parts.foreach { partRaw =>
  var list = Vector.empty[Int]

  // Scala string interpolation cannot handle escaped characters
  require(!partRaw.contains("%"))
  val part = partRaw.replace('"', '%')
  val lines = split("\n", part)
  val id = lines.head.dropRight(1).toInt
  println(id)
  lines.drop(1).foreach { str =>
    list = list :+ str.toInt
  }
  println(list)
  players = players :+ list
}

var P1 = 0
var P2 = 1

def calcScore(p: Vector[Int]): BigInt = {
  var score = BigInt(0)
  p.reverse.zipWithIndex.foreach {
    case (v, i) =>
      score = score + (BigInt(v) * (i + 1))
  }
  score
}

def combat(game: Vector[Vector[Int]]): (Int, BigInt) = {
  var history = Set.empty[Vector[Vector[Int]]]

  var cur = game
  while (true) {
    val guard = history.contains(cur)
    history = history + cur

    val canRec = !guard && cur.forall { p =>
      p.nonEmpty && p.size - 1 >= p.head
    }

    val winnerOfSubGame = if (canRec) {
      val rec = cur.map { p =>
        p.slice(1, p.head + 1)
      }
      Some(combat(rec)._1)
    } else {
      None
    }

    var prime = Vector.empty[Vector[Int]]
    var top = Vector.empty[Int]

    (guard, cur(P1).isEmpty, cur(P2).isEmpty) match {
      case (true, _, _) =>
        return (P1, calcScore(cur(P1)))
      case (_, true, false) =>
        return (P2, calcScore(cur(P2)))
      case (_, false, true) =>
        return (P1, calcScore(cur(P1)))
      case _ =>
      // skip
    }

    cur.foreach {
      case h +: r =>
        top = top :+ h
        prime = prime :+ r
      case _ =>
        throw new RuntimeException()
    }
    if (winnerOfSubGame.getOrElse(-1) == P1
      || (winnerOfSubGame.getOrElse(-1) != P2 && top.head > top.last)) {

      prime = prime.updated(P1, prime(P1) ++ top)
    } else {
      prime = prime.updated(P2, prime(P2) ++ top.reverse)
    }

    cur = prime
  }
  throw new RuntimeException()
}

println(combat(players))
