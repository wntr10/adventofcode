import scala.annotation.tailrec

// See Algorithms by Jeff Erickson

trait SearchBag[A] {
  def isEmpty: Boolean

  def head: A

  def drop: SearchBag[A]

  def updated(e: A): SearchBag[A]

  def updatedAndMarked(e: A): SearchBag[A]

  def isUnmarked(e: A): Boolean

  def marked(e: A): SearchBag[A]
}

@tailrec
def whateverFirstSearch[A](bag: SearchBag[A])(out: A => Iterable[A]): Unit = {

  if (bag.isEmpty) return
  val h = bag.head
  var bagPrime = bag.drop

  if (bag.isUnmarked(h)) {
    bagPrime = bagPrime.marked(h)
    out(h).foreach { n =>
      bagPrime = bagPrime.updated(n)
    }
  }

  whateverFirstSearch(bagPrime)(out)
}

@tailrec
def eagerWfs[A](bag: SearchBag[A])(out: A => Iterable[A], parent: (A, A) => Unit): Unit = {

  if (bag.isEmpty) return
  val v = bag.head
  var bagPrime = bag.drop

  require(!bag.isUnmarked(v))

  out(v).foreach { w =>
    if (bag.isUnmarked(w)) {
      bagPrime = bagPrime.updatedAndMarked(w)
      parent(w, v)
    }
  }

  eagerWfs(bagPrime)(out, parent)
}


def dfs[A](v: A)(isUnmarked: A => Boolean, mark: A => Unit, out: A => Iterable[A]): Unit = {
  if (isUnmarked(v)) {
    mark(v)
    out(v).foreach { w =>
      dfs(w)(isUnmarked, mark, out)
    }
  }
}


def dfsSlightlyFaster[A](v: A)(isUnmarked: A => Boolean,
                               mark: A => Unit,
                               out: A => Iterable[A],
                               parent: (A, A) => Unit,
                               preVisit: A => Unit,
                               postVisit: A => Unit): Unit = {

  mark(v)
  preVisit(v)
  out(v).foreach { w =>
    if (isUnmarked(w)) {
      parent(w, v)
      dfsSlightlyFaster(w)(isUnmarked, mark, out, parent, preVisit, postVisit)
    }
  }
  postVisit(v)
}
