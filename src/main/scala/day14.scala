object day14 extends App {

  val input = "380621"
  val inputInt = input.toInt

  case class RecipeList(scoreboard: Vector[Int] = Vector(3, 7), elf1: Int = 0, elf2: Int = 1) {
    def cook: RecipeList = {
      val newRecipes = (scoreboard(elf1) + scoreboard(elf2)).toString.map(_.asDigit).toList
      val newScoreboard = scoreboard ++ newRecipes
      val newElf1 = (1 + scoreboard(elf1) + elf1) % newScoreboard.size
      val newElf2 = (1 + scoreboard(elf2) + elf2) % newScoreboard.size
      RecipeList(newScoreboard, newElf1, newElf2)
    }

    def containsInput: Boolean = {
      val tail = scoreboard.slice((scoreboard.size - input.toString.length - 1).max(0), scoreboard.size).mkString
      tail.contains(input)
    }
  }

  val part1 = Iterator.iterate(RecipeList())(_.cook)
      .dropWhile(_.scoreboard.size < inputInt + 10)
      .next
      .scoreboard
      .slice(inputInt, inputInt + 10)
      .mkString
  println(s"PART 1: $part1")

  val part2 = Iterator.iterate(RecipeList())(_.cook)
      .dropWhile(!_.containsInput)
      .next
      .scoreboard
      .mkString
      .indexOf(input)
  println(s"PART 2: $part2")
}
