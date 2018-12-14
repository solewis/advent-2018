import scala.annotation.tailrec
import scala.io.Source

object day1 extends App {

  val input = Source.fromResource("day1.txt").getLines()
  val frequencyChanges = input.map(_.toInt).toVector

  val part1 = frequencyChanges.sum
  println(s"PART 1: $part1")

  @tailrec
  def findFirstDuplicate(changes: Vector[Int], currentFreq: Int, position: Int = 1, seen: Set[Int] = Set()): Int = {
    if (seen.contains(currentFreq)) currentFreq
    else findFirstDuplicate(changes, currentFreq + changes(position % changes.size), position + 1, seen + currentFreq)
  }

  val part2 = findFirstDuplicate(frequencyChanges, frequencyChanges.head)
  println(s"PART 2: $part2")
}
