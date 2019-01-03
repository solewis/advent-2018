import scala.io.Source

object day13 extends App {

  /* Models */
  case class Point(x: Int, y: Int) {
    def move(direction: CartDirection): Point = direction.state match {
      case "v" => Point(x, y + 1)
      case ">" => Point(x + 1, y)
      case "<" => Point(x - 1, y)
      case "^" => Point(x, y - 1)
    }
  }

  case class CartDirection(state: String) {
    def turnRight: CartDirection = CartDirection(state match {
      case "v" => "<"
      case ">" => "v"
      case "<" => "^"
      case "^" => ">"
    })

    def turnLeft: CartDirection = turnRight.turnRight.turnRight
  }

  case class Cart(coordinate: Point, cartDirection: CartDirection, nextTurn: String = "l") {
    def move: Cart = {
      val newCoordinate = coordinate.move(cartDirection)
      val newCartDirection = trackMap(newCoordinate) match {
        case "-" | "|" => cartDirection
        case "\\" => cartDirection.state match {
          case "v" | "^" => cartDirection.turnLeft
          case "<" | ">" => cartDirection.turnRight
        }
        case "/" => cartDirection.state match {
          case "v" | "^" => cartDirection.turnRight
          case "<" | ">" => cartDirection.turnLeft
        }
        case "+" => nextTurn match {
          case "l" => cartDirection.turnLeft
          case "s" => cartDirection
          case "r" => cartDirection.turnRight
        }
      }
      val newNextTurn = if (trackMap(newCoordinate) == "+") nextTurn match {
        case "l" => "s"
        case "s" => "r"
        case "r" => "l"
      } else nextTurn
      Cart(newCoordinate, newCartDirection, newNextTurn)
    }
  }

  case class CartState(carts: List[Cart], firstCrash: Option[Point] = None) {
    def tick: CartState = {
      carts.foldLeft(this) { case (previousCartState, nextCart) =>
        if (previousCartState.carts.contains(nextCart)) {
          val updatedCarts = nextCart.move :: previousCartState.carts.filterNot(_ == nextCart)
          val crashedCarts = updatedCarts
            .groupBy(_.coordinate)
            .collect { case (_, c@List(_, _)) => c }
            .flatten
            .toList
          val updatedCartsWithoutCrashes = updatedCarts
            .filterNot(crashedCarts.contains)
            .sortBy(c => (c.coordinate.y, c.coordinate.x))
          CartState(updatedCartsWithoutCrashes, previousCartState.firstCrash.orElse(crashedCarts.headOption.map(_.coordinate)))
        } else previousCartState
      }
    }
  }

  /* Input parsing */
  val input = Source.fromResource("day13.txt").getLines()

  val initialTracks = input.toList

  val cartReplacement = (_: Char) match {
    case '<' | '>' => "-"
    case 'v' | '^' => "|"
    case other => other.toString
  }
  val trackMap = initialTracks
    .zipWithIndex
    .flatMap { case (row, y) => row.zipWithIndex.map { case (track, x) => Point(x, y) -> cartReplacement(track) } }
    .toMap

  val toCart = (x: Int, y: Int, state: String) => state match {
    case "<" | ">" | "v" | "^" => Some(Cart(Point(x, y), CartDirection(state)))
    case _ => None
  }
  val initialCarts = initialTracks
    .zipWithIndex
    .flatMap { case (row, y) => row.zipWithIndex.flatMap { case (state, x) => toCart(x, y, state.toString) } }
    .sortBy(c => (c.coordinate.y, c.coordinate.x))

  /* Solutions */
  val part1 = Iterator.iterate(CartState(initialCarts))(_.tick)
    .dropWhile(_.firstCrash.isEmpty)
    .next
    .firstCrash

  println(s"PART 1: $part1")

  val part2 = Iterator.iterate(CartState(initialCarts))(_.tick)
    .dropWhile(_.carts.size > 1)
    .next
    .carts
    .headOption
    .map(_.coordinate)

  println(s"PART 2: $part2")
}