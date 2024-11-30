import $ivy.`org.jgrapht:jgrapht-core:1.5.2`
import $ivy.`com.google.guava:guava:33.3.1-jre`
import $file.^.Basic
import Basic.Input._
import $file.^.Bags
import Bags._

val ex = ".ex0" // 5

val input = read(s"day21$ex")
val lines = split("\n", input)

var rest = 0
var reverse = Map.empty[String, Set[String]]
var bag = Bag.empty[String]

lines.foreach {
  case s"$str (contains $c)" =>
    val ingredients = split(" ", str)
    val allergens = split(",", c)

    bag = bag.addAll(ingredients: _*)

    allergens.foreach { allergen =>
      val y = reverse.getOrElse(allergen, ingredients.toSet)
      reverse = reverse.updated(allergen, y.intersect(ingredients.toSet))
    }
  case r =>
    rest = rest + 1
    println(s"!<$r>")
}

require(rest == 0)
println("Reverse: " + reverse)

var solve = reverse

var done = Set.empty[String]
var stop = false

while (!stop) {
  val one = solve.filter(e => !done.contains(e._1) && e._2.size == 1)
  if (one.nonEmpty) {
    val (allergen, ingredients) = one.head
    done = done + allergen
    val ingredient = ingredients.head
    solve = solve.map { l =>
      if (l != one.head) {
        (l._1, l._2 - ingredient)
      } else {
        l
      }
    }
  } else {
    stop = true
  }
}

println(solve)
val f = solve.values.toSet.flatten

println(bag.sumBy(e => !f.contains(e)))

// Two
println(solve.toList.sortBy(e => e._1).map(_._2.head).mkString(","))
