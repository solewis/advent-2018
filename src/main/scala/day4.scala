import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import scala.io.Source

object day4 extends App {

  val input = Source.fromResource("day4.txt").getLines.toList

  case class Shift(id: Int, startTime: LocalDateTime, sleepTimes: Seq[LocalDateTime] = Nil, wakeTimes: Seq[LocalDateTime] = Nil)

  case class Guard(id: Int, shifts: Seq[Shift]) {
    val allSleepTimes: Seq[LocalDateTime] = shifts.flatMap(_.sleepTimes)
    val allWakeTimes: Seq[LocalDateTime] = shifts.flatMap(_.wakeTimes)
    val allSleepMinutes: Seq[Int] = allSleepTimes.zip(allWakeTimes).flatMap { case (sleep, wake) => sleep.getMinute until wake.getMinute }
    val totalMinutesAsleep: Long = allSleepMinutes.size
    val mostFrequentSleepMinute: Int = if (allSleepMinutes.isEmpty) -1 else allSleepMinutes.groupBy(identity).mapValues(_.size).maxBy(_._2)._1
    val mostFrequentSleepMinuteCount: Int = if (allSleepMinutes.isEmpty) -1 else allSleepMinutes.groupBy(identity).mapValues(_.size).maxBy(_._2)._2
  }

  case class Entry(time: LocalDateTime, id: Option[Int], action: Int) { //action 1=start, 2=sleep, 3=wake
    override def toString: String = action match {
      case 1 => s"[$time] Guard #${id.get} begins shift"
      case 2 => s"[$time] falls asleep"
      case 3 => s"[$time] wakes up"
    }
  }

  val startRegex =
    """\[(.*?)\] Guard #(\d+) begins shift""".r
  val sleepRegex = """\[(.*?)\] falls asleep""".r
  val wakeRegex = """\[(.*?)\] wakes up""".r

  implicit val localDateOrdering: Ordering[LocalDateTime] = _ compareTo _
  val pattern = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")

  val entries = input.map {
    case startRegex(time, id) => Entry(LocalDateTime.parse(time, pattern), Some(id.toInt), 1)
    case sleepRegex(time) => Entry(LocalDateTime.parse(time, pattern), None, 2)
    case wakeRegex(time) => Entry(LocalDateTime.parse(time, pattern), None, 3)
  }.sortBy(_.time)

  val shifts = entries.foldLeft(List[Shift]()) {
    case (acc, Entry(time, Some(id), 1)) => Shift(id, time) :: acc
    case (acc, Entry(time, None, 2)) => acc.head.copy(sleepTimes = acc.head.sleepTimes :+ time) :: acc.tail
    case (acc, Entry(time, None, 3)) => acc.head.copy(wakeTimes = acc.head.wakeTimes :+ time) :: acc.tail
  }

  val guards = shifts.groupBy(_.id).map(Guard.tupled)

  val longestSleeper = guards.toVector.maxBy(_.totalMinutesAsleep)

  val part1 = longestSleeper.id * longestSleeper.mostFrequentSleepMinute

  println(s"PART 1: $part1")

  val longestSleeperInSingleMinute = guards.toVector.maxBy(_.mostFrequentSleepMinuteCount)

  val part2 = longestSleeperInSingleMinute.id * longestSleeperInSingleMinute.mostFrequentSleepMinute

  println(s"PART 2: $part2")
}
