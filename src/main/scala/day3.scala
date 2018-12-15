import scala.io.Source

object day3 extends App {

  val input = Source.fromResource("day3.txt").getLines()

  case class Claim(id: Int, x: Int, y: Int, width: Int, height: Int) {
    val allCoordinates: Set[(Int, Int)] = (x until (x + width)).flatMap { x => (y until (y + height)).map { y => x -> y }}.toSet
  }

  val claimRegex = """#(\d+) @ (\d+),(\d+): (\d+)x(\d+)""".r

  val claims = input.toVector.map {
    case claimRegex(id, x, y, width, height) => Claim(id.toInt, x.toInt, y.toInt, width.toInt, height.toInt)
  }

  val part1 = claims.flatMap(_.allCoordinates).groupBy(identity).count(_._2.length > 1)
  println(s"PART 1: $part1")

  val noSharedCoordinates = (claim1: Claim, claim2: Claim) => claim1.allCoordinates.intersect(claim2.allCoordinates).isEmpty
  val isAlone = (claim: Claim) => claims.filter(_ != claim).forall(noSharedCoordinates(_, claim))
  val part2 = claims.find(isAlone).get.id

  println(s"PART 2: $part2")
}
