package advent2018

import advent2018.day18.LumberArea
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class Day18Spec extends FlatSpec with Matchers {

  "acre next generation an open acre" should "become trees if three or more adjacent areas have trees, otherwise remain the same" in {
    val lumberArea = LumberArea(Vector())
    val neighbors1 = List.fill(3)("|")
    lumberArea.nextGeneration(".", neighbors1) shouldBe "|"
    lumberArea.nextGeneration(".", Nil) shouldBe "."
  }

  "acre next generation trees" should "become a lumberyard if three or more adjacent areas are lumberyards, otherwise remain the same" in {
    val lumberArea = LumberArea(Vector())
    val neighbors1 = List.fill(3)("#")
    lumberArea.nextGeneration("|", neighbors1) shouldBe "#"
    lumberArea.nextGeneration("|", Nil) shouldBe "|"
  }

  "acre next generation lumberyard" should "remain a lumberyard if adjacent to at least one lumberyard and one tree, otherwise become open" in {
    val lumberArea = LumberArea(Vector())
    val neighbors1 = List("#", "|")
    lumberArea.nextGeneration("#", neighbors1) shouldBe "#"
    lumberArea.nextGeneration("#", Nil) shouldBe "."
  }

  "sample lumber area" should "have a resource value of 1147" in {
    val input =
      """
        |.#.#...|#.
        |.....#|##|
        |.|..|...#.
        |..|#.....#
        |#.#|||#|#|
        |...#.||...
        |.|....|...
        |||...#|.#|
        ||.||||..|.
        |...#.|..|.
      """.stripMargin.trim
    day18.part1(Source.fromString(input).getLines.toList) shouldBe 1147
  }

  "real lumber area" should "have a resource value of 737800" in {
    day18.part1(Source.fromResource("day18.txt").getLines.toList) shouldBe 737800
  }
}
