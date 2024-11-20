import $file.^.Basic
import Basic._

val conf = 10 // 50
val ex = ".ex0"
val input = Input.read(s"day18$ex")

type Acre = Char

case class POINT(x: Int, y: Int) {
  val valid = x >= 0 && x < conf && y >= 0 && y < conf
}

var map = Map.empty[POINT, Acre]

input.split("\n").zipWithIndex.foreach { yz =>
  yz._1.zipWithIndex.foreach { xz =>
    if (xz._1 != '.') {
      map = map.updated(POINT(xz._2, yz._2), xz._1)
    }
  }
}

var stop = false
var i = 0L

// for part two I looked at the log; found the cycle and calculated the result

while (!stop) {
  println("-".repeat(10) + i + "-".repeat(10))
  log(map)
  println("resourceValue: " + resourceValue(map))
  var prime = Map.empty[POINT, Acre]

  var world = map.keys
  world = world ++ world.flatMap(p => neighborsSet(p))
  world.foreach { e =>

    val (o, t, l) = ctx(map)(neighborsSet(e))
    val pp: Acre = (map.getOrElse(e, '.'), o, t, l) match {
      case ('.', _, tc, _) if tc >= 3 => '|'
      case ('.', _, _, _) => '.'
      case ('|', _, _, lc) if lc >= 3 => '#'
      case ('|', _, _, _) => '|'
      case ('#', _, tc, lc) if lc != 0 && tc != 0 => '#'
      case ('#', _, _, _) => '.'
    }
    if (pp != '.') {
      prime = prime.updated(e, pp)
    }
  }
  map = prime

  i = i + 1
  if (i > 10) {
    stop = true
  }
}

def ctx(map: Map[POINT, Acre])(n: Set[POINT]): (Int, Int, Int) = {
  // .toList since we need to count
  val mm = n.toList.map(p => map.getOrElse(p, '.'))
  val o = mm.count(p => p == '.')
  val t = mm.count(p => p == '|')
  val l = mm.count(p => p == '#')
  (o, t, l)
}

def neighborsSet(p: POINT): Set[POINT] = {
  var c = Set.empty[POINT]
  List(-1, 0, 1).foreach { dx =>
    List(-1, 0, 1).foreach { dy =>
      val pp = POINT(p.x + dx, p.y + dy)
      if (p != pp && pp.valid) {
        c = c + pp
      }
    }
  }
  require(c.size <= 8)
  c
}

def resourceValue(map: Map[POINT, Acre]): Int = {
  val tr = map.count(p => p._2 == '|')
  val lu = map.count(p => p._2 == '#')
  tr * lu
}

def log(map: Map[POINT, Acre]): Unit = {
  val x = map.map(_._1.x)
  val y = map.map(_._1.y)
  val minX = x.min
  val minY = y.min
  val maxX = x.max
  val maxY = y.max

  var cy = minY
  while (cy < maxY + 1) {
    var cx = minX
    while (cx < maxX + 1) {
      val po = POINT(cx, cy)
      val p = map.getOrElse(po, '.')
      print(p)
      cx = cx + 1
    }
    println(s" $cy")
    cy = cy + 1
  }
}
