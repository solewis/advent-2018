import scala.io.Source

object day5 extends App {

  val input = Source.fromResource("day5.txt").mkString

  val reactors = ('a' to 'z').zip('A' to 'Z').flatMap { case (upper, lower) => Seq(upper.toString + lower, lower.toString + upper) }

  def react(str: String, position: Int = 0): String = {
    if (str.length < position + 2) str
    else if (reactors.contains(str.substring(position, position + 2)))
      react(str.substring(0, position) + str.substring(position + 2), (position - 1).max(0))
    else react(str, position + 1)
  }

  val part1 = react(input).length
  println(s"PART 1: $part1")

  val part2 = ('a' to 'z')
    .map(ch => input.replace(ch.toString, "").replace(ch.toUpper.toString, ""))
    .map(react(_))
    .minBy(_.length)
    .length
  println(s"PART 2: $part2")
}
