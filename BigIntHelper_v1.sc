
object BigIntHelper {

  def vec(e: Int*): Vector[BigInt] = {
    e.toVector.map(BigInt(_))
  }

}
