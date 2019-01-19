package advent2018

import scala.annotation.tailrec
import scala.io.Source

object day10 extends App {

  val input = Source.fromResource("day10.txt").getLines()

  case class Point(x: Int, y: Int) {
    def +(other: Point): Point = Point(x + other.x, y + other.y)
  }

  case class LightSignal(coordinates: Point, velocity: Point)

  val signalRegex = """position=<([- ]?\d+), ([- ]?\d+)> velocity=<([- ]?\d+), ([- ]?\d+)>""".r
  val signals = input.map { case signalRegex(x, y, xV, yV) =>
    LightSignal(Point(x.trim.toInt, y.trim.toInt), Point(xV.trim.toInt, yV.trim.toInt))
  }.toList

  case class SignalMap(signals: List[LightSignal]) {
    val xRange: Range.Inclusive = signals.map(_.coordinates.x).min to signals.map(_.coordinates.x).max
    val yRange: Range.Inclusive = signals.map(_.coordinates.y).min to signals.map(_.coordinates.y).max
    val area: Long = xRange.size.toLong * yRange.size.toLong

    def next: SignalMap = SignalMap(signals.map(s => LightSignal(s.coordinates + s.velocity, s.velocity)))

    def printMap(): Unit = {
      val on = signals.map(_.coordinates).toSet
      yRange.foreach(y => println(xRange.map(x => if (on.contains(Point(x, y))) "#" else ".").mkString))
    }
  }

  val initialMap = SignalMap(signals)
  val mapIterator = Iterator.iterate(initialMap)(_.next)

  @tailrec
  def smallestMap(current: SignalMap, maps: Iterator[SignalMap], count: Long = 0): (SignalMap, Long) = {
    val next = maps.next
    if (current.area < next.area) current -> count else smallestMap(next, maps, count + 1)
  }

  val (smallestSignalMap, count) = smallestMap(mapIterator.next, mapIterator)

  //part1
  smallestSignalMap.printMap()
  println(s"PART 2: $count")
}
