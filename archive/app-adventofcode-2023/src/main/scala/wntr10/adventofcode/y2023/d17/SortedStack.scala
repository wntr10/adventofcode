package wntr10.adventofcode.y2023.d17

import scala.collection.mutable.ListBuffer

final class SortedStack[K] {
  private implicit val ordering: Ordering[Long] = Ordering.Long.reverse
  private val pq = collection.mutable.PriorityQueue.empty[Long]
  private val map = scala.collection.mutable.Map.empty[Long, ListBuffer[K]]
  private val EMPTY = ListBuffer.empty

  def peek(): Option[Long] = {
    pq.headOption
  }

  def push(prio: Long, key: K): Unit = {
    pq.addOne(prio)
    val stackOpt = map.get(prio)
    val stack = if (stackOpt.isEmpty) {
      val lb = ListBuffer.empty[K]
      map.put(prio, lb)
      lb
    } else {
      stackOpt.get
    }
    stack.prepend(key)
  }

  def pop(): Option[K] = {
    if (pq.nonEmpty) {
      var prio = pq.dequeue()
      var stack = map.getOrElse(prio, EMPTY)
      while (stack.isEmpty && pq.nonEmpty) {
        prio = pq.dequeue()
        stack = map.getOrElse(prio, EMPTY)
      }
      val size = stack.size
      if (stack.size > 1) {
        val h = stack.head
        stack.remove(0)
        Some(h)
      } else if (size == 1) {
        map.remove(prio)
        Some(stack.head)
      } else {
        None
      }
    } else {
      None
    }
  }

  def update(from: Long, to: Long, key: K): Unit = {
    map(from).subtractOne(key)
    push(to, key)
  }

  def validate(): Boolean = {
    pq.size >= map.values.map(_.size).sum
  }

}
