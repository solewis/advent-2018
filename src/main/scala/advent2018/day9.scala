package advent2018

import java.util

object day9 extends App {

  val NUM_PLAYERS = 428
  val NUM_TURNS = 7206100

  case class Game(playerScores: Map[Int, Long], board: StackBoard = StackBoard(), currentMarble: Int = 1) {
    def takeTurn: Game = {
      if (currentMarble % 23 == 0) {
        val (newBoard, removed) = board.remove
        val currentPlayer = currentMarble % playerScores.size
        Game(
          playerScores = playerScores + (currentPlayer -> (playerScores(currentPlayer) + currentMarble + removed)),
          board = newBoard,
          currentMarble = currentMarble + 1
        )
      } else {
        Game(
          playerScores = playerScores,
          board = board.add(currentMarble),
          currentMarble = currentMarble + 1
        )
      }
    }

    def highScore: Long = playerScores.values.max
  }

  //Part 1 implementation
  case class Board(items: List[Int] = List(0), currentMarbleIndex: Int = 0) {
    def add(marble: Int): Board =
      if (currentMarbleIndex == items.size - 1) Board(items.head :: marble :: items.tail, 1)
      else Board((items.slice(0, currentMarbleIndex + 2) :+ marble) ++ items.slice(currentMarbleIndex + 2, items.size), currentMarbleIndex + 2)

    def remove: (Board, Int) = {
      val index = ((currentMarbleIndex - 7) + items.size) % items.size
      Board(items.slice(0, index) ++ items.slice(index + 1, items.size), index) -> items(index)
    }
  }

  val startingStack = new util.Stack[Int]
  startingStack.push(0)

  //Part 2 implementation
  case class StackBoard(leftItems: util.Stack[Int] = startingStack, rightItems: util.Stack[Int] = new util.Stack) {
    def add(marble: Int): StackBoard = {
      if (rightItems.isEmpty) moveCurrentMarbleIndexLeft(leftItems.size - 1)
      else moveCurrentMarbleIndexRight()
      leftItems.push(marble)
      this
    }

    def remove: (StackBoard, Int) = {
      if (leftItems.size < 7) moveCurrentMarbleIndexRight(rightItems.size - 7 + leftItems.size)
      else moveCurrentMarbleIndexLeft(7)
      val removed = leftItems.pop
      moveCurrentMarbleIndexRight()
      this -> removed
    }

    def moveCurrentMarbleIndexRight(num: Int = 1): Unit = (1 to num).foreach(_ => leftItems.push(rightItems.pop))

    def moveCurrentMarbleIndexLeft(num: Int = 1): Unit = (1 to num).foreach(_ => rightItems.push(leftItems.pop))
  }

  val playerScores = (0 until NUM_PLAYERS).map(_ -> 0L).toMap
  val part1 = Iterator.iterate(Game(playerScores))(_.takeTurn).drop(NUM_TURNS).next.highScore

  println(s"PART 1: $part1")
}
