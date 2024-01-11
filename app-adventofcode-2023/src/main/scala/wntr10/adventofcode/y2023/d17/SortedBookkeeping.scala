package wntr10.adventofcode.y2023.d17

class SortedBookkeeping[K, V](ordering: Ordering[V]) {
  val sorted = new SortedStack[K]
  private val open = scala.collection.mutable.Map.empty[K, (Long, V)]
  val closed = scala.collection.mutable.Map.empty[K, V]

  def add(prio: Long, key: K, value: V): Unit = {
    sorted.push(prio, key)
    open.put(key, (prio, value))
  }

  def peek(): Option[Long] = {
    sorted.peek()
  }

  def contains(key: K): Boolean = {
    open.contains(key)
  }

  def update(prio: Long, key: K, value: V): Unit = {
    val entry = open(key)
    if (ordering.compare(value, entry._2) < 0) {
      sorted.update(open(key)._1, prio, key)
      open.put(key, (prio, value))
    }
  }

  def filter(keys: Set[K]): Set[K] = {
    keys.diff(closed.keySet)
  }

  def pop(): Option[(K, V)] = {
    val keyOpt = sorted.pop()
    if (keyOpt.isDefined) {
      val key = keyOpt.get
      val value = open(keyOpt.get)
      open.remove(key)
      closed.put(key, value._2)
      Some(key, value._2)
    } else {
      None
    }
  }

}
