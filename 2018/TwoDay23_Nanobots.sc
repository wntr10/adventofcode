import $file.^.Basic
import Basic._
import Input._
import $file.^.Grid_v3
import Grid_v3._

import scala.collection.mutable

val ex = ".ex0" // 36
val inputRaw = read(s"day23$ex")
val lines = splitOn("\n")(inputRaw)

case class BOT(x: BigInt, y: BigInt, z: BigInt, r: BigInt) {

  def comp(c: Int): BigInt = {
    c match {
      case 0 => x
      case 1 => y
      case 2 => z
    }
  }

  def intersects(rs: Vector[R]): Boolean = {
    rs.zipWithIndex.map(e => e._1.distance(comp(e._2))).sum <= r
  }
}

type LINE = BOT
var prime = Vector.empty[LINE]
var countRest = 0

def visit(line: String, idx: BigInt): Unit = {
  (line, idx) match {
    case (s"pos=<$x,$y,$z>, r=$r", _) =>
      prime = prime :+ BOT(BigInt(x), BigInt(y), BigInt(z), BigInt(r))
    case (l, i) =>
      countRest = countRest + 1
      println(s"REST ${pad(i)}: <$l>")
  }
}

lines.zipWithIndex.foreach {
  case (lines, idx) =>
    visit(lines, idx)
}

require(countRest == 0)

def run(): Unit = {

  val x = R.minMax(prime.map(_.x))
  val y = R.minMax(prime.map(_.y))
  val z = R.minMax(prime.map(_.z))

  case class CANDIDATE(box: Vector[R], axis: Int) {
    lazy val weight: BigInt = prime.count(n => n.intersects(box))

    def component: R = box(axis)
  }

  implicit val ordering: Ordering[CANDIDATE] = new Ordering[CANDIDATE] {
    override def compare(x: CANDIDATE, y: CANDIDATE): Int = x.weight.compare(y.weight)
  }

  val queue = mutable.PriorityQueue.empty
  val v = Vector(x, y, z)
  queue.enqueue(CANDIDATE(v, 0))

  while (true) {
    val h = queue.dequeue()

    if (h.box.forall(_.singleton)) {
      println(h.box.map(_.lower).sum)
      return
    }

    val nextAxis = (h.axis + 1) % 3

    val c = h.component
    if (c.singleton) {
      queue.enqueue(h.copy(axis = nextAxis))
    } else {
      val lr = c.split
      val a = h.box.updated(h.axis, lr.head)
      val b = h.box.updated(h.axis, lr.last)
      queue.enqueue(CANDIDATE(a, nextAxis))
      queue.enqueue(CANDIDATE(b, nextAxis))
    }
  }
}

run()
