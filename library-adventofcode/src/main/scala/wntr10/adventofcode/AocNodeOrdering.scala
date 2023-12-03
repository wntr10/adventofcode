package wntr10.adventofcode

object AocNodeOrdering extends Ordering[AocNode] {

  override def compare(o1: AocNode, o2: AocNode): Int = o1.key.str.compareTo(o2.key.str)
}
