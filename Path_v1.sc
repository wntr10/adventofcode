import scala.collection.immutable.SortedMap

final case class E[T](prev: Set[T], g: BigInt, done: Boolean = false) {
  def close(): E[T] = {
    require(!done)
    copy(done = true)
  }
}

final case class B[T](cost: Map[T, E[T]], queue: SortedMap[BigInt, List[T]]) {

  def path(to: T): Set[Vector[T]] = {

    def pathRec(pa: Set[Vector[T]]): Set[Vector[T]] = {
      var ps = Set.empty[Vector[T]]

      pa.foreach { p =>
        val h = p.head
        val ce = cost(h)
        if (ce.prev == Set(h)) {
          ps = ps + p
        } else {
          ps = ps ++ pathRec(ce.prev.map { pre => pre +: p })
        }
      }
      ps
    }

    pathRec(Set(Vector(to)))
  }

  def next(): (B[T], Option[(BigInt, T)]) = {
    var q = queue
    while (q.nonEmpty) {
      var (c, candidates) = q.head
      q = q.drop(1)
      while (candidates.nonEmpty) {
        val p = candidates.head
        candidates = candidates.drop(1)
        val pe = cost(p)
        if (!pe.done) {
          if (candidates.nonEmpty) {
            q = q.updated(c, candidates)
          }
          return (copy(queue = q, cost = cost.updated(p, pe.close())), Some((pe.g, p)))
        }
      }
    }
    (copy(queue = q), None)
  }

  def merge(prev: T, g: BigInt, p: T, all: Boolean = false): B[T] = {
    var costPrime = cost
    var queuePrime = queue

    cost.get(p) match {
      case Some(e) if e.g == g && all =>
        costPrime = costPrime.updated(p, E(e.prev + prev, g))
      case Some(e) if e.g <= g => // skip
      case _ =>
        costPrime = costPrime.updated(p, E(Set(prev), g))
        val peers = queuePrime.getOrElse(g, List.empty)
        queuePrime = queuePrime.updated(g, p :: peers)
    }
    B(cost = costPrime, queue = queuePrime)
  }
}
