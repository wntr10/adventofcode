

def mergeSort[A](seq: Seq[A])(ordering: Ordering[A]): Seq[A] = {

  def merge(left: Seq[A], right: Seq[A]): Seq[A] = {
    var l = left
    var r = right
    var prime = Seq.empty[A]
    while(l.nonEmpty || r.nonEmpty) {
      (l, r) match {
        case (_, Seq()) =>
          prime = prime ++ l
          l = Seq.empty
        case (Seq(), _) =>
          prime = prime ++ r
          r = Seq.empty
        case (lh +: lr, rh +: _) if ordering.compare(lh, rh) <= 0 =>
          prime = prime :+ lh
          l = lr
        case (_, rh +: rr) =>
          prime = prime :+ rh
          r = rr
      }
    }
    prime
  }

  if (seq.size > 1) {
    val mid = seq.size >> 1
    val left = mergeSort(seq.take(mid))(ordering)
    val right = mergeSort(seq.drop(mid))(ordering)
    merge(left, right)
  } else {
    seq
  }
}
