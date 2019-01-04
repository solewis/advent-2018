object day14 extends App {

  val input = "380621"
  val inputInt = input.toInt

  def buildRecipes(scoreboard: Vector[Int] = Vector(3, 7), elf1: Int = 0, elf2: Int = 1): Vector[Int] = {
    if (scoreboard.size > inputInt + 10) scoreboard
    else {
      val newRecipes = (scoreboard(elf1) + scoreboard(elf2)).toString.map(_.asDigit).toList
      val newScoreboard = scoreboard ++ newRecipes
      val newElf1 = (1 + scoreboard(elf1) + elf1) % newScoreboard.size
      val newElf2 = (1 + scoreboard(elf2) + elf2) % newScoreboard.size
      buildRecipes(newScoreboard, newElf1, newElf2)
    }
  }

  val part1 = buildRecipes().slice(inputInt, inputInt + 10).mkString
  println(s"PART 1: $part1")

  def buildRecipes2(scoreboard: Vector[Int] = Vector(3, 7), elf1: Int = 0, elf2: Int = 1): Int = {
    val tail = scoreboard.slice((scoreboard.size - input.toString.length - 1).max(0), scoreboard.size).mkString
    if (tail.contains(input)) scoreboard.mkString.indexOf(input)
    else {
      val newRecipes = (scoreboard(elf1) + scoreboard(elf2)).toString.map(_.asDigit).toList
      val newScoreboard = scoreboard ++ newRecipes
      val newElf1 = (1 + scoreboard(elf1) + elf1) % newScoreboard.size
      val newElf2 = (1 + scoreboard(elf2) + elf2) % newScoreboard.size
      buildRecipes2(newScoreboard, newElf1, newElf2)
    }
  }

  val part2 = buildRecipes2() //~10 seconds...
  println(s"PART 2: $part2")
}
