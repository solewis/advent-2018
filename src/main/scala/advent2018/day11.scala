package advent2018

object day11 extends App {

  val SERIAL_NUMBER = 8444

  case class Point(x: Int, y: Int)

  val xRange = 1 to 300
  val yRange = 1 to 300

  def powerLevel(x: Int, y: Int): Int = {
    val rackId = x + 10
    (((rackId * y + SERIAL_NUMBER) * rackId) / 100) % 10 - 5
  }

  val coordinatePowerMap = xRange.flatMap(x => yRange.map(y => Point(x, y) -> powerLevel(x, y))).toMap

  case class GridPower(coordinate: Point, size: Int, power: Int) {
    def nextSize: GridPower = {
      val additionalBottomRowPower = (coordinate.x to coordinate.x + size).map(x => coordinatePowerMap(Point(x, coordinate.y + size))).sum
      val additionalRightRowPower = (coordinate.y until coordinate.y + size).map(y => coordinatePowerMap(Point(coordinate.x + size, y))).sum
      GridPower(coordinate, size + 1, power + additionalBottomRowPower + additionalRightRowPower)
    }
  }

  def gridPowers(x: Int, y: Int): Seq[GridPower] = {
    val largestPossibleSize = Seq(300 - x, 300 - y).min
    val oneByOne = GridPower(Point(x, y), 1, coordinatePowerMap(Point(x, y)))
    Iterator.iterate(oneByOne)(_.nextSize).take(largestPossibleSize + 1).toList
  }

  val gridPowers: Seq[GridPower] = xRange.flatMap(x => yRange.flatMap(y => gridPowers(x, y)))

  val part1 = gridPowers
    .filter(_.size == 3)
    .maxBy(_.power)
    .coordinate

  println(s"PART 1: $part1")
  //PART 1: 243,68

  val part2 = gridPowers
    .maxBy(_.power)

  println(s"PART 2: $part2")
  //PART 2: 236,252,12
}
