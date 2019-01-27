package advent2018

import advent2018.day17.{Grid, Point}
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class Day17Spec extends FlatSpec with Matchers {

  "expand" should "return itself if it is stuck between two clay spots" in {
    val grid = Grid(Set(Point(0, 0), Point(2, 0), Point(1, 1)), 0 to 0, 0 to 0)
    grid.expand(Point(1, 0), Set()) should be(List(Point(1, 0)))
  }

  "expand" should "expand left if stuck next to clay on right" in {
    val grid = Grid(Set(Point(0, 0), Point(3, 0), Point(1, 1), Point(2, 1)), 0 to 0, 0 to 0)
    grid.expand(Point(2, 0), Set()) should contain theSameElementsAs List(Point(1, 0), Point(2, 0))
  }

  "expand" should "expand right if stuck next to clay on left" in {
    val grid = Grid(Set(Point(0, 0), Point(3, 0), Point(1, 1), Point(2, 1)), 0 to 0, 0 to 0)
    grid.expand(Point(1, 0), Set()) should contain theSameElementsAs List(Point(1, 0), Point(2, 0))
  }

  "expand" should "expand both ways if in open area" in {
    val grid = Grid(Set(Point(0, 0), Point(4, 0), Point(1, 1), Point(2, 1), Point(3, 1)), 0 to 0, 0 to 0)
    grid.expand(Point(2, 0), Set()) should contain theSameElementsAs List(Point(1, 0), Point(2, 0), Point(3, 0))
  }

  "expand" should "only expand while there is clay or water underneath" in {
    val grid = Grid(Set(Point(1, 1)), 0 to 0, 0 to 0)
    grid.expand(Point(1, 0), Set(Point(2, 1))) should contain theSameElementsAs List(Point(1, 0), Point(2, 0))
  }

  "simulate drip" should "fill bucket and go down each side" in {
    /**
      * ...+...     |||||||
      * .#...#. --> |#~~~#|
      * .#...#.     |#~~~#|
      * .#####.     |#####|
      **/
    val grid = Grid(Set(Point(1, 1), Point(1, 2), Point(1, 3), Point(2, 3),
      Point(3, 3), Point(4, 3), Point(5, 3), Point(5, 2), Point(5, 1)), 0 to 6, 0 to 3)

    val water = grid.simulateDrip(Point(3, 0))
    grid.printGrid(water)
    water.toList.sortBy(w => (w.y, w.x)) should contain theSameElementsAs List(
      Point(0, 0), Point(1, 0), Point(2, 0), Point(3, 0), Point(4, 0), Point(5, 0), Point(6, 0),
      Point(0, 1), Point(0, 2), Point(0, 3), Point(6, 1), Point(6, 2), Point(6, 3),
      Point(2, 1), Point(2, 2), Point(3, 1), Point(3, 2), Point(4, 1), Point(4, 2)
    ).sortBy(w => (w.y, w.x))
  }

  "simulate drip" should "hit edge of bucket and fill bucket" in {
    /**
      * .+....#     .~.....
      * ......#     .~.....
      * .....#. --> ~~~~~#.
      * .#...#.     ~#~~~#.
      * .#####.     ~#####.
      **/
    val inputString =
      """
        |x=1, y=3..4
        |y=4, x=2..5
        |x=5, y=2..3
        |x=6, y=0..1
      """.stripMargin.trim
    val input = Source.fromString(inputString).getLines().toList
    val grid = day17.parseGrid(input)
    grid.simulateDrip(Point(1, 0)).size should be(12)
  }

  "simulate drip" should "hit edge of bucket inside bucket and fill both buckets" in {
    /**
      * ...+....##        ...~....##
      * ..........        ~~~~~~~~~~
      * .#......#.        ~#~~~~~~#~
      * .#.#..#.#.        ~#~#~~#~#~
      * .#.####.#.        ~#~####~#~
      * .#......#.        ~#~~~~~~#~
      * .########.        ~########~
      **/
    val inputString =
      """
        |y=0, x=8..9
        |y=6, x=1..8
        |x=1, y=2..5
        |x=8, y=2..5
        |x=3, y=3..4
        |x=6, y=3..4
        |y=4, x=4..5
      """.stripMargin.trim
    val input = Source.fromString(inputString).getLines().toList
    val grid = day17.parseGrid(input)
    val water = grid.simulateDrip(Point(3, 0))
    grid.printGrid(water)
    water.size should be(39)
  }

  "simulate drip" should "not duplicate drips if two drips hit same bucket" in {
    /**
      * ##....+....##     ##..~~~~~..##
      * .....#.#.....     ....~#~#~....
      * .....#.#.....     ....~#~#~....
      * .....###.....     ....~###~....
      * ............. --> .~~~~~~~~~~~.
      * ..#.......#..     .~#~~~~~~~#~.
      * ..#.......#..     .~#~~~~~~~#~.
      * ..#########..     .~#########~.13 11 14 6
      **/
    val inputString =
      """
        |y=0, x=0..1
        |y=0, x=11..12
        |y=3, x=5..7
        |y=7, x=2..10
        |x=5, y=1..3
        |x=7, y=1..3
        |x=2, y=5..6
        |x=10, y=5..6
      """.stripMargin.trim
    val input = Source.fromString(inputString).getLines().toList
    val grid = day17.parseGrid(input)
    val water = grid.simulateDrip(Point(6, 0))
    grid.printGrid(water)
    water.size should be(44)
  }

  "the sample scan" should "have water reach 57 spots" in {
    /**
      * ......+.......      ......+.......
      * ............#.      ......|.....#.
      * .#..#.......#.      .#..#||||...#.
      * .#..#..#......      .#..#~~#|.....
      * .#..#..#......      .#..#~~#|.....
      * .#.....#......      .#~~~~~#|.....
      * .#.....#...... -->  .#~~~~~#|.....
      * .#######......      .#######|.....
      * ..............      ........|.....
      * ..............      ...|||||||||..
      * ....#.....#...      ...|#~~~~~#|..
      * ....#.....#...      ...|#~~~~~#|..
      * ....#.....#...      ...|#~~~~~#|..
      * ....#######...      ...|#######|..
      **/
    val inputString =
      """
        |x=495, y=2..7
        |y=7, x=495..501
        |x=501, y=3..7
        |x=498, y=2..4
        |x=506, y=1..2
        |x=498, y=10..13
        |x=504, y=10..13
        |y=13, x=498..504
      """.stripMargin.trim
    val input = Source.fromString(inputString).getLines().toList
    day17.part1(input) shouldBe 57
  }

  "the real scan" should "have water reach correct spots" in {
    val input = Source.fromResource("day17.txt").getLines().toList
    day17.part1(input) shouldBe 31471
  }

  "the sample scan" should "have 29 tiles of water after the spring runs dry" in {
    val inputString =
      """
        |x=495, y=2..7
        |y=7, x=495..501
        |x=501, y=3..7
        |x=498, y=2..4
        |x=506, y=1..2
        |x=498, y=10..13
        |x=504, y=10..13
        |y=13, x=498..504
      """.stripMargin.trim
    val input = Source.fromString(inputString).getLines().toList
    day17.part2(input) shouldBe 29
  }

  "the real scan" should "have correct number of tiles of water after the spring runs dry" in {
    val input = Source.fromResource("day17.txt").getLines().toList
    day17.part2(input) shouldBe 24169
  }
}
