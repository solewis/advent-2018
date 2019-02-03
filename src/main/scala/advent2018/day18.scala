package advent2018

object day18 {

  case class Point(x: Int, y: Int) {
    def adjacentPoints: List[Point] = {
      ((y - 1) to (y + 1)).flatMap { y => ((x - 1) to (x + 1)).map { x => Point(x, y) } }
        .filterNot(_ == this)
        .toList
    }
  }

  case class Acre(point: Point, contents: String) {
    def nextGeneration(neighbors: List[Acre]): Acre = {
      this.copy(contents = contents match {
        case "." => if (neighbors.count(_.contents == "|") >= 3) "|" else "."
        case "|" => if (neighbors.count(_.contents == "#") >= 3) "#" else "|"
        case "#" => if (neighbors.exists(_.contents == "#") && neighbors.exists(_.contents == "|")) "#" else "."
      })
    }
  }

  case class LumberArea(acres: List[Acre]) {
    val yRange: Range = acres.map(_.point.y).indices
    val xRange: Range = acres.map(_.point.x).indices

    def printLumberArea(): Unit = {
      acres
        .sortBy(a => (a.point.y, a.point.x))
        .grouped(xRange.size)
        .map(_.map(_.contents).mkString)
        .foreach(println)
    }

    def simulateMinute: LumberArea = {
      LumberArea(acres.map(acre => acre.nextGeneration(adjacentAcres(acre.point))))
    }

    def adjacentAcres(point: Point): List[Acre] = {
      val adjacentPoints = point.adjacentPoints
        .filter(p => yRange.contains(p.y) && xRange.contains(p.x))
        .toSet
      acres.filter(acre => adjacentPoints.contains(acre.point))
    }

    def woodedAreas: Int = acres.count(_.contents == "|")

    def lumberyards: Int = acres.count(_.contents == "#")
  }

  def parseInput(input: List[String]): LumberArea = {
    LumberArea(input.zipWithIndex.flatMap { case (line, y) =>
      line.zipWithIndex.map { case (contents, x) =>
        Acre(Point(x, y), contents.toString)
      }
    })
  }

  def part1(input: List[String]): Int = {
    val lumberArea = parseInput(input)
    val lumberArea10Minutes = Iterator.iterate(lumberArea)(_.simulateMinute).take(10).next
    lumberArea10Minutes.printLumberArea()
    lumberArea10Minutes.woodedAreas * lumberArea10Minutes.lumberyards
  }
}
