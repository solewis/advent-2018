import scala.io.Source

object day6 extends App {

  case class Coordinate(x: Int, y: Int)

  val input = Source.fromResource("day6.txt").getLines()

  val inputRegex = """(\d+), (\d+)""".r

  val mappedCoordinates = input.map { case inputRegex(x, y) => Coordinate(x.toInt, y.toInt) }.toList

  val leftMost = mappedCoordinates.map(_.x).min - 1
  val topMost = mappedCoordinates.map(_.y).min - 1
  val rightMost = mappedCoordinates.map(_.x).max + 1
  val bottomMost = mappedCoordinates.map(_.y).max + 1

  //Form a square that surrounds all mapped coordinates, the graph is a list of all those points
  val graph = (leftMost to rightMost).flatMap(x => (topMost to bottomMost).map(y => Coordinate(x, y)))

  def manhattanDistance(a: Coordinate, b: Coordinate): Int = Math.abs(a.x - b.x) + Math.abs(a.y - b.y)

  def closestCoordinate(point: Coordinate): Option[Coordinate] = {
    mappedCoordinates
      .map(coordinate => coordinate -> manhattanDistance(point, coordinate))
      .sortBy(_._2) match {
      case a :: b :: _ if a._2 != b._2 => Some(a._1)
      case _ => None
    }
  }

  val closestMappedCoordinates = graph.map(point => point -> closestCoordinate(point))

  //Any mapped coordinate which is the closest point on the edge of the graph is assumed infinite area
  val infiniteMappedCoordinates = closestMappedCoordinates.filter { case (point, _) =>
    point.x == leftMost || point.x == rightMost || point.y == topMost || point.y == bottomMost
  }.flatMap(_._2).toSet

  val finiteMappedCoordinates = mappedCoordinates.filterNot(infiniteMappedCoordinates.contains)

  def area(coordinate: Coordinate): Int = closestMappedCoordinates.count(_._2.contains(coordinate))

  val part1 = finiteMappedCoordinates.map(area).max
  println(s"PART 1: $part1")

  def totalDistance(point: Coordinate): Int = mappedCoordinates.map(manhattanDistance(_, point)).sum

  val part2 = graph.map(totalDistance).count(_ < 10000)
  println(s"PART 2: $part2")
}
