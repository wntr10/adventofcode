package wntr10.adventofcode.y2019.d12

import com.google.gson.Gson
import wntr10.adventofcode.Input

object PartOne extends App {

  private val gson = new Gson()

  private case class Line(x: Int, y: Int, z: Int, vx: Int, vy: Int, vz: Int) {
    private def potentialEnergy: Int = x.abs + y.abs + z.abs

    private def kineticEnergy: Int = vx.abs + vy.abs + vz.abs

    def totalEnergy: Int = potentialEnergy * kineticEnergy
  }

  private lazy val input: List[Line] = {
    val suffix = ""
    //val suffix = ".ex0"
    val input = new Input(this.getClass.getName, suffix)
    val lines = input.read
      .replace('\n', ';')
      .replace('=', ':')
      .replace('<', '{')
      .replace('>', '}')
      .replace("x", "\"x\"")
      .replace("y", "\"y\"")
      .replace("z", "\"z\"")

    lines.split(';').toList.map { l =>
      println(l)
      gson.fromJson[Line](l, classOf[Line])
    }
  }

  private[this] def solve(input: List[Line]): Unit = {

    var moons = input

    for (_ <- Range.Int(0, 1000, 1)) {

      moons = moons.map { ml =>
        var dx = 0
        var dy = 0
        var dz = 0
        moons.foreach { mr =>
          dx = dx + mr.x.compareTo(ml.x)
          dy = dy + mr.y.compareTo(ml.y)
          dz = dz + mr.z.compareTo(ml.z)
        }
        ml.copy(vx = ml.vx + dx, vy = ml.vy + dy, vz = ml.vz + dz)
      }

      moons = moons.map { m =>
        m.copy(x = m.x + m.vx, y = m.y + m.vy, z = m.z + m.vz)
      }
    }

    println(moons.map(m => m.totalEnergy).sum)

  }

  solve(input)

}
