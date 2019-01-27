package advent2018

object day17 extends App {

  case class Point(x: Int, y: Int) {
    def left: Point = Point(x - 1, y)

    def right: Point = Point(x + 1, y)

    def up: Point = Point(x, y - 1)

    def down: Point = Point(x, y + 1)
  }

  case class Grid(clay: Set[Point], xRange: Range, yRange: Range) {
    val clayByColumn: Map[Int, List[Point]] = clay
      .groupBy(_.x)
      .mapValues(_.toList)
      .mapValues(_.sortBy(_.y))

    def printGrid(water: Set[Point]): Unit = {
      yRange.foreach { y =>
        val line = (xRange.min - 1 to xRange.max + 1).map { x =>
          if ((x, y) == (500, yRange.min)) "+"
          else if (clay.contains(Point(x, y))) "#"
          else if (water.contains(Point(x, y))) "~"
          else "."
        }.mkString
        println(line)
      }
      println()
    }

    def firstDrip: Point = Point(500, yRange.min)

    def simulateDrip(drip: Point = firstDrip): Set[Point] = {
      val (dripWater, fillWater) = simulateDripHelper(drip)
      dripWater ++ fillWater
    }

    def simulateDripHelper(drip: Point = firstDrip, dripWater: Set[Point] = Set(), fillWater: Set[Point] = Set()): (Set[Point], Set[Point]) = {
      //find first clay below the drip if it exists
      val nextClay = clayByColumn.get(drip.x).flatMap(_.find(_.y > drip.y))

      nextClay.map { clay =>
        //Fill bucket until overflow
        val (nextDrips, newWater) = fill(clay.up, fillWater)
        val (newDripWater, newFillWater) = newWater.partition(_.y == nextDrips.head.y)
        val updatedDripWater = (drip.y until clay.y).map(Point(drip.x, _)).toSet ++ dripWater ++ newDripWater
        val updatedFillWater = fillWater ++ newFillWater
        val allWater = updatedDripWater ++ updatedFillWater
        nextDrips.sortBy(_.x) match {
          case List(a) => if (!allWater.contains(a)) simulateDripHelper(a, updatedDripWater, updatedFillWater) else updatedDripWater -> updatedFillWater
          case List(a, b) =>
            val (firstDrip, firstFill) = if (!allWater.contains(a)) simulateDripHelper(a, updatedDripWater, updatedFillWater) else updatedDripWater -> updatedFillWater
            if (!(firstDrip ++ firstFill).contains(b)) simulateDripHelper(b, firstDrip, firstFill) else firstDrip -> firstFill
        }
      }.getOrElse {
        //Drip flowed off grid
        ((drip.y to yRange.max).map(Point(drip.x, _)).toSet ++ dripWater) -> fillWater
      }
    }

    def fill(from: Point, allFillWater: Set[Point], newFillWater: Set[Point] = Set()): (List[Point], Set[Point]) = {
      val expandedWater = expand(from, allFillWater).sortBy(_.x)
      (expandedWater.head, expandedWater.last) match {
        case (l, r) if clay.contains(l.left) && clay.contains(r.right) => fill(from.up, allFillWater ++ expandedWater, newFillWater ++ expandedWater)
        case (l, r) if clay.contains(l.left) => List(r.right) -> (newFillWater ++ expandedWater)
        case (l, r) if clay.contains(r.right) => List(l.left) -> (newFillWater ++ expandedWater)
        case (l, r) => List(l.left, r.right) -> (newFillWater ++ expandedWater)
      }
    }

    def expand(from: Point, accumulatedWater: Set[Point]): List[Point] = {
      def expandHelper(from: Point, direction: Point => Point): List[Point] = {
        val supportedUnderneath = (p: Point) => clay.contains(p.down) || accumulatedWater.contains(p.down)
        Iterator.iterate(from)(direction)
          .takeWhile(p => !clay.contains(p) && supportedUnderneath(p))
          .toList
      }

      val left = expandHelper(from, _.left)
      val right = expandHelper(from, _.right)
      left ++ right.tail //tail to drop the duplicated center point
    }
  }

  def parseGrid(input: List[String]): Grid = {
    val rowRegex = """(\w)=(\d+), [xy]=(\d+)..(\d+)""".r
    val clay = input.flatMap {
      case rowRegex(firstChar, value, rangeStart, rangeEnd) =>
        (rangeStart.toInt to rangeEnd.toInt).map { xy =>
          if (firstChar == "x") Point(value.toInt, xy)
          else Point(xy, value.toInt)
        }
    }
    val xRange = clay.map(_.x).min to clay.map(_.x).max
    val yRange = clay.map(_.y).min to clay.map(_.y).max
    Grid(clay.toSet, xRange, yRange)
  }

  def part1(input: List[String]): Int = {
    val grid = parseGrid(input)
    val water = grid.simulateDrip()
    grid.printGrid(water)
    water.size
  }

  def part2(input: List[String]): Int = {
    val grid = parseGrid(input)
    val (_, fillWater) = grid.simulateDripHelper()
    grid.printGrid(fillWater)
    fillWater.size
  }
}
