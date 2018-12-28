import scala.io.Source

object day12 extends App {

  /* Read input */
  val input = Source.fromResource("day12.txt").getLines()

  val initialState = input.next.split(" ")(2).zipWithIndex.map { case (state, index) => PlantPot(index, state.toString) }.toVector
  input.next
  val ruleRegex = """([.#]+) => ([.#])""".r
  val ruleMap = input.map { case ruleRegex(pattern, result) => pattern -> result }.toMap

  /* Models */
  case class PlantPot(potId: Int, plantState: String) {
    def evolve(pattern: String): PlantPot = PlantPot(potId, ruleMap.getOrElse(pattern, "."))
  }

  case class Generation(plants: Vector[PlantPot]) {
    def patternFor(index: Int): String = List(
      plants.lift(index - 2).map(_.plantState),
      plants.lift(index - 1).map(_.plantState),
      plants.lift(index).map(_.plantState),
      plants.lift(index + 1).map(_.plantState),
      plants.lift(index + 2).map(_.plantState)
    ).flatten.mkString

    def next: Generation = Generation(plants
      .zipWithIndex
      .map { case (plant, index) => plant.evolve(patternFor(index)) }
      .padWithEmptyPots)
  }

  /* Implicits */
  implicit class PlantList(plants: Vector[PlantPot]) {
    def padWithEmptyPots: Vector[PlantPot] = {
      //Ensure plant list always has 5 empty pots on each end
      val leftPadSize = 5 - plants.takeWhile(_.plantState == ".").size
      val rightPadSize = 5 - plants.reverse.takeWhile(_.plantState == ".").size
      val leftPad = ((plants.head.potId - leftPadSize) until plants.head.potId).map(id => PlantPot(id, ".")).toVector
      val rightPad = ((plants.last.potId + 1) to (plants.last.potId + rightPadSize)).map(id => PlantPot(id, ".")).toVector
      leftPad ++ plants ++ rightPad
    }
  }

  /* Solutions */
  val gen0 = Generation(initialState.padWithEmptyPots)

  val part1 = Iterator.iterate(gen0)(_.next)
    .drop(20)
    .next
    .plants
    .filter(_.plantState == "#")
    .map(_.potId)
    .sum
  println(s"PART 1: $part1")

  val sumAt200 = Iterator.iterate(gen0)(_.next)
    .drop(200)
    .next
    .plants
    .filter(_.plantState == "#")
    .map(_.potId)
    .sum
  //pattern stabilizes at some before 200 generations, and the pot id sum increases by 20 every gen after
  val part2 = sumAt200 + ((50000000000L - 200) * 20)
  println(s"PART 2: $part2")
}
