import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import scala.io.Source

object day4 extends App {

  val input = Source.fromResource("day4.txt").getLines.toList

  case class Shift(id: Int, sleepTimes: Seq[LocalDateTime] = Nil, wakeTimes: Seq[LocalDateTime] = Nil)

  case class Guard(id: Int, shifts: Seq[Shift]) {
    val allSleepTimes: Seq[LocalDateTime] = shifts.flatMap(_.sleepTimes)
    val allWakeTimes: Seq[LocalDateTime] = shifts.flatMap(_.wakeTimes)
    val allSleepMinutes: Seq[Int] = allSleepTimes.zip(allWakeTimes).flatMap { case (sleep, wake) => sleep.getMinute until wake.getMinute }
    val totalMinutesAsleep: Long = allSleepMinutes.size
    val mostFrequentSleepMinute: Int = if (allSleepMinutes.isEmpty) -1 else allSleepMinutes.groupBy(identity).mapValues(_.size).maxBy(_._2)._1
    val mostFrequentSleepMinuteCount: Int = if (allSleepMinutes.isEmpty) -1 else allSleepMinutes.groupBy(identity).mapValues(_.size).maxBy(_._2)._2
  }

  case class Entry(time: LocalDateTime, id: Option[Int], action: Int) //action 1=start, 2=sleep, 3=wake

  val startRegex = """\[(.*?)\] Guard #(\d+) begins shift""".r
  val sleepRegex = """\[(.*?)\] falls asleep""".r
  val wakeRegex = """\[(.*?)\] wakes up""".r

  val pattern = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")
  val shifts = input.sorted.foldLeft(List[Shift]()) {
    case (acc, startRegex(_, id)) => Shift(id.toInt) :: acc
    case (acc, sleepRegex(time)) => acc.head.copy(sleepTimes = acc.head.sleepTimes :+ LocalDateTime.parse(time, pattern)) :: acc.tail
    case (acc, wakeRegex(time)) => acc.head.copy(wakeTimes = acc.head.wakeTimes :+ LocalDateTime.parse(time, pattern)) :: acc.tail
  }

  val guards = shifts.groupBy(_.id).map(Guard.tupled)

  val longestSleeper = guards.toVector.maxBy(_.totalMinutesAsleep)

  val part1 = longestSleeper.id * longestSleeper.mostFrequentSleepMinute

  println(s"PART 1: $part1")

  val longestSleeperInSingleMinute = guards.toVector.maxBy(_.mostFrequentSleepMinuteCount)

  val part2 = longestSleeperInSingleMinute.id * longestSleeperInSingleMinute.mostFrequentSleepMinute

  println(s"PART 2: $part2")
}
