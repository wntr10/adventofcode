package wntr10.adventofcode.y2023.d20

import wntr10.adventofcode.Input

object PartTwo extends App {

  private lazy val input: Parser.Alpha = {
    // Modify the input so that only one of the clusters is processed.
    // (Use graphviz to print the system.)
    // Then determine the number of button presses for each one.
    // In my case it results in
    // 3847, 4057, 3769, 3877
    // Finally, multiply

    val suffix = ".ex5"
    val input = new Input(this.getClass.getName, suffix)
    val lines = input.read

    Parser.parse(lines)
  }

  lazy val length: Int = input.length

  private var modules = Map.empty[String, Module]

  type Pulse = Boolean

  trait Module {

    var halt = false

    private var initialized = false

    var input = List.empty[(String, Pulse)]

    val name: String

    val destinations: List[String]

    def receive(from: String, pulse: Pulse): Unit = {
      input = (from, pulse) :: input
    }

    def receiveInit(from: String): Unit = {
      init()
    }

    def step(): Unit = {
      val tmp = input
      input = List.empty[(String, Pulse)]
      tmp.foreach { i =>
        process(i._1, i._2)
      }
    }

    def process(from: String, pulse: Pulse): Unit = {
      send(pulse)
    }

    def send(pulse: Pulse): Unit = {
      destinations.foreach { d =>
        modules.get(d).foreach(_.receive(name, pulse))
      }
    }

    def init(): Unit = {
      if (!initialized) {
        initialized = true
        destinations.foreach { d =>
          modules.get(d).foreach(_.receiveInit(name))
        }
      }
    }
  }

  private case class Broadcast(destinations: List[String]) extends Module {
    val name = "broadcaster"
  }

  private case class FlipFlop(name: String, destinations: List[String]) extends Module {
    private var on = false

    override def process(from: String, pulse: Pulse): Unit = {
      if (!pulse) {
        if (on) {
          send(pulse)
        } else {
          send(!pulse)
        }
        on = !on
      }
    }
  }

  private case class Conjunction(name: String, destinations: List[String]) extends Module {
    private var memory = Map.empty[String, Boolean]

    override def receiveInit(from: String): Unit = {
      memory = memory.updated(from, false)
      init()
    }

    override def process(from: String, pulse: Pulse): Unit = {
      memory = memory.updated(from, pulse)
      if (memory.values.forall(v => v)) {
        send(false)
      } else {
        send(true)
      }
    }
  }

  private case class Rx(name: String = "rx", destinations: List[String] = List.empty) extends Module {
    override def process(from: String, pulse: Pulse): Unit = {
      if (!pulse) {
        halt = true
      }
    }
  }

  private def solve(input: Parser.Alpha): BigInt = {

    input.foreach { l =>
      val a = l.a
      val b = l.b.toList

      a match {
        case s"%$n" =>
          modules = modules.updated(n, FlipFlop(n, b))
        case s"&$n" =>
          modules = modules.updated(n, Conjunction(n, b))
        case n =>
          modules = modules.updated(n, Broadcast(b))
      }
    }

    modules = modules.updated("rx", Rx())

    modules("broadcaster").init()

    var presses = BigInt(0)
    while (modules.values.forall(m => !m.halt)) {
      presses += 1

      modules("broadcaster").receive("button", pulse = false)

      while (modules.values.exists(m => m.input.nonEmpty)) {
        modules.values.foreach(m => m.step())
      }
    }

    presses
  }

  println(solve(input))

}
