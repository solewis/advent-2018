import scala.io.Source

object day6 extends App {

  case class Coordinate(x: Int, y: Int) {
    def manhattanDistance(other: Coordinate): Int = Math.abs(x - other.x) + Math.abs(y - other.y)
  }

  val input = Source.fromResource("day6.txt").getLines()
  val inputRegex = """(\d+), (\d+)""".r
  val mappedCoordinates = input.map { case inputRegex(x, y) => Coordinate(x.toInt, y.toInt) }.toList

  val xRange = mappedCoordinates.map(_.x).min to mappedCoordinates.map(_.x).max
  val yRange = mappedCoordinates.map(_.y).min to mappedCoordinates.map(_.y).max

  //Form a square that surrounds all mapped coordinates, the graph is a list of all those points
  val graph = xRange.flatMap(x => yRange.map(y => Coordinate(x, y)))

  def closestCoordinate(point: Coordinate): Option[Coordinate] = {
    val List(a, b, _*) = mappedCoordinates.sortBy(_.manhattanDistance(point))
    if (a.manhattanDistance(point) == b.manhattanDistance(point)) None else Some(a)
  }

  def isOnEdge(point: Coordinate): Boolean = point.x == xRange.start || point.x == xRange.end || point.y == yRange.start || point.y == yRange.end
  val infiniteMappedCoordinates = graph.filter(isOnEdge).flatMap(closestCoordinate).toSet

  val part1_2 = graph
    .flatMap(closestCoordinate)
    .filterNot(infiniteMappedCoordinates.contains)
    .groupBy(identity)
    .map(_._2.size)
    .max

  println(s"PART 1: $part1_2")

  def totalDistance(point: Coordinate): Int = mappedCoordinates.map(_.manhattanDistance(point)).sum

  val part2 = graph
    .map(totalDistance)
    .count(_ < 10000)
  println(s"PART 2: $part2")
}
