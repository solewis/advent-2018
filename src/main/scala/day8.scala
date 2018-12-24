import scala.io.Source

object day8 extends App {

  val input = Source.fromResource("day8.txt").getLines.next.split(" ").map(_.toInt).toIterator

  case class Node(children: List[Node], metadata: List[Int]) {
    def totalMetadata: Int = metadata.sum + children.map(_.totalMetadata).sum

    def totalValue: Int = if (children.isEmpty) totalMetadata else metadata.flatMap(m => children.lift(m - 1).map(_.totalValue)).sum
  }

  def parseNode(input: Iterator[Int]): Node = {
    val numChildren = input.next
    val numMetadata = input.next
    val children = (0 until numChildren).map(_ => parseNode(input))
    val metadata = (0 until numMetadata).map(_ => input.next)
    Node(children.toList, metadata.toList)
  }

  val headNode = parseNode(input)

  val part1 = headNode.totalMetadata
  println(s"PART 1: $part1")

  val part2 = headNode.totalValue
  println(s"PART 2: $part2")
}
