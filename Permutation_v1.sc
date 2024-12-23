
object Permutation {

  def bronKerbosch[T](visit: Set[T] => Unit, neighborSet: T => Set[T])
                     (r: Set[T], _p: Set[T], _x: Set[T]): Unit = {

    if (_p.isEmpty && _x.isEmpty) {
      visit(r)
    } else {
      var p = _p
      var x = _x
      p.foreach { v =>
        val n = neighborSet(v)
        bronKerbosch(visit, neighborSet)(r + v, p.intersect(n), x.intersect(n))
        p -= v
        x += v
      }
    }
  }

  def subSets[T](visit: Set[T] => Unit)
                (master: Set[T], x: Set[T]): Unit = {

    if (master.isEmpty) {
      visit(x)
    } else {
      val pick = master.head
      subSets(visit)(master.drop(1), x + pick)
      subSets(visit)(master.drop(1), x)
    }
  }

  private def even(k: Int): Boolean = {
    (k & 1) == 0
  }

  private def swap[T](a: Vector[T], b: Int, c: Int): Vector[T] = {
    a.updated(c, a(b)).updated(b, a(c))
  }

  def heap[T](k: Int, a: Vector[T], visit: Vector[T] => Unit): Unit = {
    if (k == 1) {
      visit(a)
    } else {
      heap(k - 1, a, visit)
      val isEven = even(k)
      var ca = a
      Range(0, k - 1).foreach { i =>
        ca = if (isEven) {
          swap(ca, i, k - 1)
        } else {
          swap(ca, 0, k - 1)
        }
        heap(k - 1, ca, visit)
      }
    }
  }

  def permute[T](visit: Vector[T] => Unit, prune: Vector[T] => Boolean)
                (v: Vector[T], chosen: Vector[T]): Unit = {

    if (v.isEmpty) {
      visit(chosen)
    } else {
      v.indices.foreach { i =>
        val (left, right) = v.splitAt(i)
        val pick = chosen :+ right.head
        if (!prune(pick)) {
          permute(visit, prune)(left ++ right.drop(1), pick)
        }
      }
    }
  }

  def permuteSets[T](visit: Vector[T] => Unit, prune: Vector[T] => Boolean)
                    (v: Vector[Set[T]], chosen: Vector[T]): Unit = {

    if (v.isEmpty) {
      visit(chosen)
    } else {
      v.indices.foreach { i =>
        val (left, right) = v.splitAt(i)
        right.head.foreach { c =>
          val pick = chosen :+ c
          if (!prune(pick)) {
            permuteSets(visit, prune)(left ++ right.drop(1), chosen :+ c)
          }
        }
      }
    }
  }

}
