package advent2018

object day18 {

  case class LumberArea(acres: Vector[Vector[String]]) {
    val yRange: Range = acres.indices
    val xRange: Range = acres.headOption.map(_.indices).getOrElse(0 to 0)

    def printLumberArea(): Unit = acres.map(_.mkString).foreach(println)

    def simulateMinute: LumberArea = {
      LumberArea(yRange.map(y =>
        xRange.map(x =>
          nextGeneration(acres(y)(x), adjacentAcres(y, x))).toVector).toVector)
    }

    def nextGeneration(contents: String, neighbors: List[String]): String = {
      contents match {
        case "." => if (neighbors.count(_ == "|") >= 3) "|" else "."
        case "|" => if (neighbors.count(_ == "#") >= 3) "#" else "|"
        case "#" => if (neighbors.contains("#") && neighbors.contains("|")) "#" else "."
      }
    }

    def adjacentAcres(y: Int, x: Int): List[String] = {
      ((y - 1) to (y + 1)).flatMap(b => ((x - 1) to (x + 1)).map(a => a -> b))
        .filterNot { case (a, b) => a == x && b == y }
        .filter { case (a, b) => xRange.contains(a) && yRange.contains(b) }
        .map { case (a, b) => acres(b)(a) }
        .toList
    }

    def woodedAreas: Int = acres.flatten.count(_ == "|")

    def lumberyards: Int = acres.flatten.count(_ == "#")
  }

  def parseInput(input: List[String]): LumberArea = {
    LumberArea(input.map(_.map(_.toString).toVector).toVector)
  }

  def part1(input: List[String]): Int = {
    val lumberArea = parseInput(input)
    val lumberArea10Minutes = Iterator.iterate(lumberArea)(_.simulateMinute).drop(10).next
    lumberArea10Minutes.printLumberArea()
    lumberArea10Minutes.woodedAreas * lumberArea10Minutes.lumberyards
  }
}
