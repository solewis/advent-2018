import scala.annotation.tailrec
import scala.io.Source

object day7 extends App {

  val input = Source.fromResource("day7.txt").getLines().toList

  val inputRegex = """Step (.) must be finished before step (.) can begin.""".r
  val stepDependents = input
    .foldLeft(List[(String, List[String])]()) { case (acc, inputRegex(a, b)) => (a -> List(b)) :: (b -> Nil) :: acc }
    .groupBy(_._1)
    .mapValues(_.flatMap(_._2))

  val stepDependencies = input
    .foldLeft(List[(String, List[String])]()) { case (acc, inputRegex(a, b)) => (a -> Nil) :: (b -> List(a)) :: acc }
    .groupBy(_._1)
    .mapValues(_.flatMap(_._2))

  def dependenciesMet(step: String, completed: String): Boolean = stepDependencies(step).forall(completed.contains)

  @tailrec
  def getOrder(availableSteps: Set[String], order: String = ""): String = {
    if (availableSteps.isEmpty) order
    else {
      val nextStep = availableSteps.filter(dependenciesMet(_, order)).min
      getOrder(availableSteps.filter(_ != nextStep) ++ stepDependents(nextStep), order + nextStep)
    }
  }

  val heads = stepDependencies.filter(_._2.isEmpty).keySet
  val part1 = getOrder(heads)
  println(s"PART 1: $part1")

  val workers: Map[Int, Option[String]] = (1 to 5).map(_ -> None).toMap

  case class Second(availableSteps: Set[String],
                    inProgressSteps: Set[(String, Int)] = Set(),
                    completed: String = "",
                    workersAvailable: Int = 5,
                    second: Int = -1) {
    def hasNext: Boolean = inProgressSteps.nonEmpty || availableSteps.nonEmpty

    def next: Second = {
      val newlyCompletedSteps = inProgressSteps.filter(_._2 == 1).map(_._1)
      val tempWorkersAvailable = workersAvailable + newlyCompletedSteps.size
      val newCompleted = completed + newlyCompletedSteps.mkString
      val newlyInProgressSteps = availableSteps.filter(dependenciesMet(_, newCompleted)).map(c => c -> (60 + c.charAt(0) - 64)).take(tempWorkersAvailable)
      val finalWorkersAvailable = tempWorkersAvailable - newlyInProgressSteps.size
      val newAvailableSteps = availableSteps.diff(newlyInProgressSteps.map(_._1)) ++ newlyInProgressSteps.map(_._1).flatMap(stepDependents)
      val newInProgressSteps = inProgressSteps.filterNot(a => newlyCompletedSteps.contains(a._1)).map(a => a._1 -> (a._2 - 1)) ++ newlyInProgressSteps
      val result = copy(
        availableSteps = newAvailableSteps,
        inProgressSteps = newInProgressSteps,
        completed = newCompleted,
        workersAvailable = finalWorkersAvailable,
        second = second + 1
      )
      result
    }
  }

  val initialSecond = Second(heads)

  val part2 = Iterator
    .iterate(initialSecond)(_.next)
    .dropWhile(_.hasNext)
    .next
    .second
  println(s"PART 2: $part2")
}
