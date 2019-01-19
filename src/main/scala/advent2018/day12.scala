package advent2018

import scala.io.Source

object day12 extends App {

  /* Read input */
  val input = Source.fromResource("day12.txt").getLines()

  val initialState = input.next.replace(".", " ")
  val ruleMap = input
    .map(_.split(" => ").map(_.replace(".", " ")).toList)
    .map { case pattern :: result :: Nil => pattern -> result }
    .toMap

  /* Models */
  case class Generation(plants: String, indexOfFirstPlant: Int) {
    def next: Generation = {
      val newPlants = s"    $plants    ".sliding(5).map(ruleMap).mkString
      Generation(
        plants = newPlants.trim,
        indexOfFirstPlant = indexOfFirstPlant - 2 + newPlants.indexOf("#")
      )
    }

    def score: Int = plants
      .zipWithIndex
      .filter(_._1 == '#')
      .map { case (_, index) => indexOfFirstPlant + index }
      .sum
  }

  /* Solutions */
  val gen0 = Generation(initialState, 0)

  val part1 = Iterator.iterate(gen0)(_.next)
    .drop(20)
    .next
    .score
  println(s"PART 1: $part1")

  val sumAt200 = Iterator.iterate(gen0)(_.next)
    .drop(200)
    .next
    .score

  //pattern stabilizes at some point before 200 generations, and the pot id sum increases by 20 every gen after
  val part2 = sumAt200 + ((50000000000L - 200) * 20)
  println(s"PART 2: $part2")
}
