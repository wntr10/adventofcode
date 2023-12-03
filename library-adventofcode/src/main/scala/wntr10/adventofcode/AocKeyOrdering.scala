package wntr10.adventofcode

object AocKeyOrdering extends Ordering[AocKey] {

  override def compare(o1: AocKey, o2: AocKey): Int = o1.str.compareTo(o2.str)
}
