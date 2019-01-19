package advent2018

import scala.annotation.tailrec
import scala.io.Source

object day1 extends App {

  val input = Source.fromResource("day1.txt").getLines()
  val frequencyChanges = input.map(_.toInt).toVector

  val part1 = frequencyChanges.sum
  println(s"PART 1: $part1")

  @tailrec
  def findFirstDuplicate(changes: Stream[Int], currentFreq: Int = 0, seen: Set[Int] = Set()): Int = {
    if (seen.contains(currentFreq)) currentFreq
    else findFirstDuplicate(changes.tail, currentFreq + changes.head, seen + currentFreq)
  }

  val part2 = findFirstDuplicate(Stream.continually(frequencyChanges.toStream).flatten)
  println(s"PART 2: $part2")
}
