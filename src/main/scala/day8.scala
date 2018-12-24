import scala.io.Source

object day8 extends App {

  val input = Source.fromResource("day8.txt").getLines.next.split(" ").toList.map(_.toInt)

  case class Node(children: Vector[Node], metadata: List[Int]) {
    def totalMetadata: Int = metadata.sum + children.map(_.totalMetadata).sum

    def totalValue: Int = if (children.isEmpty) totalMetadata else metadata.map(m => children.lift(m - 1).map(_.totalValue).getOrElse(0)).sum
  }

  def parseNodes(numNodes: Int, input: List[Int]): (List[Node], List[Int]) = {
    (0 until numNodes).foldLeft(List[Node](), input) { case ((nodes, remainingInput), _) =>
      val numChildren :: numMetadata :: tail = remainingInput
      val (children, newRemainingInput) = parseNodes(numChildren, tail)
      (nodes :+ Node(children.toVector, newRemainingInput.take(numMetadata))) -> newRemainingInput.drop(numMetadata)
    }
  }

  val headNode = parseNodes(1, input)._1.head

  val part1 = headNode.totalMetadata
  println(s"PART 1: $part1")

  val part2 = headNode.totalValue
  println(s"PART 2: $part2")
}
