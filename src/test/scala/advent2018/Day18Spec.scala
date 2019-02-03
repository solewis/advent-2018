package advent2018

import advent2018.day18.{Acre, Point}
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class Day18Spec extends FlatSpec with Matchers {

  "open adjacent spots" should "return all 8 spots around given spot" in {
    val p = Point(1, 1)
    p.adjacentPoints shouldBe List(
      Point(0, 0),
      Point(1, 0),
      Point(2, 0),
      Point(0, 1),
      Point(2, 1),
      Point(0, 2),
      Point(1, 2),
      Point(2, 2)
    )
  }

  "acre next generation an open acre" should "become trees if three or more adjacent areas have trees, otherwise remain the same" in {
    val acre1 = Acre(Point(1, 1), ".")
    val neighbors1 = List.fill(3)(Acre(Point(0, 0), "|"))
    acre1.nextGeneration(neighbors1).contents shouldBe "|"

    val acre2 = Acre(Point(1, 1), ".")
    val neighbors2 = List.fill(2)(Acre(Point(0, 0), "|"))
    acre2.nextGeneration(neighbors2).contents shouldBe "."
  }

  "acre next generation trees" should "become a lumberyard if three or more adjacent areas are lumberyards, otherwise remain the same" in {
    val acre1 = Acre(Point(1, 1), "|")
    val neighbors1 = List.fill(3)(Acre(Point(0, 0), "#"))
    acre1.nextGeneration(neighbors1).contents shouldBe "#"

    val acre2 = Acre(Point(1, 1), "|")
    val neighbors2 = List.fill(2)(Acre(Point(0, 0), "#"))
    acre2.nextGeneration(neighbors2).contents shouldBe "|"
  }

  "acre next generation lumberyard" should "remain a lumberyard if adjacent to at least one lumberyard and one tree, otherwise become open" in {
    val acre1 = Acre(Point(1, 1), "#")
    val neighbors1 = List(Acre(Point(0, 0), "#"), Acre(Point(0, 0), "|"))
    acre1.nextGeneration(neighbors1).contents shouldBe "#"

    val acre2 = Acre(Point(1, 1), "#")
    acre2.nextGeneration(Nil).contents shouldBe "."
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
    day18.part1(Source.fromString(input).getLines().toList) shouldBe 1147
  }
}
