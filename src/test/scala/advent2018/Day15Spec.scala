package advent2018

import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class Day15Spec extends FlatSpec with Matchers {

  "Real input" should "pass" in {
    val input = Source.fromResource("day15.txt").getLines().toList
    day15.part1(input) should be (195774)
    day15.part2(input) should be (37272)
  }

  "Sample 1" should "pass" in {
    val grid =
      """
        |#######
        |#.G...#
        |#...EG#
        |#.#.#G#
        |#..G#E#
        |#.....#
        |#######
      """.stripMargin.trim
    val input = Source.fromString(grid).getLines().toList
    day15.part1(input) should be (27730)
    day15.part2(input) should be (4988)
  }

  "Sample 2" should "pass" in {
    val grid =
      """
        |#######
        |#G..#E#
        |#E#E.E#
        |#G.##.#
        |#...#E#
        |#...E.#
        |#######
      """.stripMargin.trim
    val input = Source.fromString(grid).getLines().toList
    day15.part1(input) should be (36334)
  }

  "Sample 3" should "pass" in {
    val grid =
      """
        |#######
        |#E..EG#
        |#.#G.E#
        |#E.##E#
        |#G..#.#
        |#..E#.#
        |#######
      """.stripMargin.trim
    val input = Source.fromString(grid).getLines().toList
    day15.part1(input) should be (39514)
    day15.part2(input) should be (31284)
  }

  "Sample 4" should "pass" in {
    val grid =
      """
        |#######
        |#E.G#.#
        |#.#G..#
        |#G.#.G#
        |#G..#.#
        |#...E.#
        |#######
      """.stripMargin.trim
    val input = Source.fromString(grid).getLines().toList
    day15.part1(input) should be (27755)
    day15.part2(input) should be (3478)
  }

  "Sample 5" should "pass" in {
    val grid =
      """
        |#######
        |#.E...#
        |#.#..G#
        |#.###.#
        |#E#G#G#
        |#...#G#
        |#######
      """.stripMargin.trim
    val input = Source.fromString(grid).getLines().toList
    day15.part1(input) should be (28944)
    day15.part2(input) should be (6474)
  }

  "Sample 6" should "pass" in {
    val grid =
      """
        |#########
        |#G......#
        |#.E.#...#
        |#..##..G#
        |#...##..#
        |#...#...#
        |#.G...G.#
        |#.....G.#
        |#########
      """.stripMargin.trim
    val input = Source.fromString(grid).getLines().toList
    day15.part1(input) should be (18740)
    day15.part2(input) should be (1140)
  }
}
