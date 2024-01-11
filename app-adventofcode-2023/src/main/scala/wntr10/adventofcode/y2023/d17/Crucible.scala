package wntr10.adventofcode.y2023.d17

import wntr10.adventofcode.{AocDirection, AocNode}

trait Crucible {
  def n: AocNode
  def dir: Option[AocDirection]
  def turnAfter: Int
  def keepDirectionFor: Int = 0
}
