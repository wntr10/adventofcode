package wntr10.adventofcode

case class AocRange(start: Long, exclusive: Long) {
  require(start <= exclusive)

  def subrange(offset: Long): AocRange = {
    this.copy(start = Math.min(start + offset, exclusive))
  }

  def rsubrange(offset: Long): AocRange = {
    this.copy(exclusive = Math.max(exclusive - offset, start))
  }

  val length: Long = exclusive - start

  def contains(value: Long): Boolean = {
    value >= start && value < exclusive
  }

  def apply(index: Int): Long = {
    val v = start + index
    require(v < exclusive)
    v
  }

  def before(range: AocRange): Boolean = {
    exclusive <= range.start
  }

  def after(range: AocRange): Boolean = {
    range.exclusive <= start
  }

  def disjoint(range: AocRange): Boolean = before(range) || after(range)

  def starts(range: AocRange): Boolean = {
    start == range.start && exclusive <= range.exclusive
  }

  def finishes(range: AocRange): Boolean = {
    start <= range.start && exclusive == range.exclusive
  }

  def during(range: AocRange): Boolean = {
    start >= range.start && exclusive <= range.exclusive
  }

  def overlap(range: AocRange): Boolean = {
    start < range.start && exclusive <= range.exclusive
  }

  def minus(range: AocRange): Set[AocRange] = {
    var set = Set.empty[AocRange]
    if (start < range.start) {
      set = set + AocRange(start, Math.min(exclusive, range.start))
    }

    if (range.exclusive < exclusive) {
      set = set + AocRange(Math.max(range.exclusive, start), exclusive)
    }
    set
  }

  def intersect(range: AocRange): Set[AocRange] = {
    if (disjoint(range)) {
      Set.empty
    } else {
      Set(AocRange(Math.max(start, range.start), Math.min(exclusive, range.exclusive)))
    }
  }

  def intersectIdx(ranges: Set[AocRange]): Set[AocRange] = {
    var i = Set.empty[AocRange]
    ranges.foreach { range =>
      if (!disjoint(range)) {
        i = i ++ Set(AocRange(Math.max(start, range.start) - start, Math.min(exclusive, range.exclusive) - start))
      }
    }
    i
  }

  def minusIdx(range: AocRange): Set[AocRange] = {
    var set = Set.empty[AocRange]
    if (start < range.start) {
      set = set + AocRange(0, Math.min(exclusive, range.start) - start)
    }

    if (range.exclusive < exclusive) {
      set = set + AocRange(Math.max(range.exclusive, start) - start, exclusive - start)
    }
    set
  }

  def apply(index: Set[AocRange]): Set[AocRange] = {
    index.map { r =>
      val startPrime = start + r.start
      require(startPrime < exclusive)
      val exclusivePrime = start + r.exclusive
      require(exclusivePrime <= exclusive)
      r.copy(start = startPrime, exclusive = exclusivePrime)
    }
  }
}

object AocRange {
  def of(start: Long, length: Long): AocRange = {
    require(length >= 0)
    AocRange(start, start + length)
  }
}
