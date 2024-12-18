import scala.jdk.CollectionConverters.ListHasAsScala
import $file.^.Basic
import com.google.common.base.Splitter

// please note that all bus ids are prime numbers

val inputs = List(
  ("7,13,x,x,59,x,31,19", "1068781"),
  ("17,x,13,19", "3417"),
  ("67,7,59,61", "754018"),
  ("67,x,7,59,61", "779210"),
  ("67,7,x,59,61", "1261476"),
  ("1789,37,47,1889", "1202161486"),
  ("13,x,x,41,x,x,x,x,x,x,x,x,x,997,x,x,x,x,x,x,x,23,x,x,x,x,x,x,x,x,x,x,19,x,x,x,x,x,x,x,x,x,29,x,619,x,x,x,x,x,37,x,x,x,x,x,x,x,x,x,x,17", "471793476184394")
)

inputs.foreach { ids =>
  println(s"== ${ids._1} ==")

  val lines = Splitter.on(",").omitEmptyStrings().trimResults().splitToList(ids._1).asScala

  val list = lines.zipWithIndex.filter(l => l._1 != "x").map(e => (BigInt(e._1), e._2))

  var timestamp = BigInt(0)
  var buses = List.empty[(BigInt, Int)]
  var stepSize = BigInt(1)

  list.foreach { bus =>
    val (id, _) = bus
    buses = bus :: buses
    timestamp = search(timestamp, stepSize, buses)
    println(s"Depart at timestamp=$timestamp")

    // central observation: factor division of next stepSize must contain all solved bus ids
    stepSize = stepSize * id
  }

  require(BigInt(ids._2) == timestamp)
}

def search(start: BigInt, stepSize: BigInt, buses: List[(BigInt, Int)]): BigInt = {
  var t = start
  while (true) {
    val i = buses.iterator
    var allDeparting = true
    while (allDeparting && i.hasNext) {
      val (id, offset) = i.next()
      allDeparting = allDeparting && (t + offset) % id == 0
    }
    if (allDeparting) {
      return t
    }
    t += stepSize
  }
  // never happen
  t
}
