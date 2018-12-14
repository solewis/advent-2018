import scala.io.Source

object day2 extends App {

  val lines = Source.fromResource("day2.txt").getLines()
  val ids = lines.toVector

  val counts = ids.foldLeft(0, 0) { case ((exactlyTwoCount, exactlyThreeCount), id) =>
    val counts = id.groupBy(identity).mapValues(_.length).values.toSet
    exactlyTwoCount + counts.count(_ == 2) -> (exactlyThreeCount + counts.count(_ == 3))
  }

  val part1 = counts._1 * counts._2
  println(s"PART 1: $part1")

  def numDifferences(str1: String, str2: String): Int =
    str1.zip(str2).count { case (char1, char2) => char1 != char2 }

  val part2 = ids.flatMap { id1 =>
    ids.flatMap { id2 =>
      if (numDifferences(id1, id2) == 1) Some(id1 intersect id2)
      else None
    }
  }.head

  println(s"PART 2: $part2")
}
