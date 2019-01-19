package advent2018

import scala.io.Source

object day3 extends App {

  val input = Source.fromResource("day3.txt").getLines()

  case class Claim(id: Int, x: Int, y: Int, width: Int, height: Int) {
    val allCoordinates: Set[(Int, Int)] = (x until x + width).flatMap(x => (y until y + height).map(x -> _)).toSet
  }

  val claimRegex = """#(\d+) @ (\d+),(\d+): (\d+)x(\d+)""".r

  val claims = input.toVector.map {
    case claimRegex(id, x, y, width, height) => Claim(id.toInt, x.toInt, y.toInt, width.toInt, height.toInt)
  }

  val pointCountMap = claims.flatMap(_.allCoordinates).groupBy(identity).mapValues(_.size)
  val part1 = pointCountMap.count(_._2 > 1)
  println(s"PART 1: $part1")

  val isAlone = (claim: Claim) => claim.allCoordinates.forall(pointCountMap(_) == 1)
  val part2 = claims.find(isAlone).get.id

  println(s"PART 2: $part2")
}
