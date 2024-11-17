package wntr10.adventofcode.y2023.d17

final class Dijkstra(sb: Crucible) {

  private val ordering: Ordering[(Crucible, BigInt)] = new Ordering[(Crucible, BigInt)] {
    override def compare(x: (Crucible, BigInt), y: (Crucible, BigInt)): Int = {
      x._2.compare(y._2)
    }
  }

  val sorted = new SortedBookkeeping[Crucible, (Crucible, BigInt)](ordering)
  sorted.add(0L, sb, (sb, 0))

  def path(po: Crucible): (BigInt, List[Crucible]) = {
    val mi = sorted.closed(po)

    var p = po
    var vor = mi._1
    var path = List(p)
    var cost = p.n.value.str.toLong
    while (vor != p) {
      p = vor
      path = p :: path
      vor = sorted.closed(p)._1
      if (vor != p) {
        cost += p.n.value.str.toLong
      }
    }
    (mi._2, path.reverse)
  }

  def peek(): Option[Long] = {
    sorted.peek()
  }

  def update(predecessor: Crucible, s: Map[Crucible, BigInt]): Unit = {
    val keys = sorted.filter(s.keySet)
    keys.foreach { k =>
      val v = s(k)
      val prio = v
      if (sorted.contains(k)) {
        sorted.update(prio.toLong, k, (predecessor, v))
      } else {
        sorted.add(prio.toLong, k, (predecessor, v))
      }
    }
  }

  def next(): Option[(Crucible, (Crucible, BigInt))] = {
    sorted.pop()
  }

}
