package advent2018

import scala.collection.immutable.Queue

object day15 extends App {

  case class Point(x: Int, y: Int) {
    def manhattanDistance(other: Point): Int = (x - other.x).abs + (y - other.y).abs

    def adjacentSpots: List[Point] = {
      List(
        Point(x - 1, y),
        Point(x + 1, y),
        Point(x, y - 1),
        Point(x, y + 1)
      )
    }

    def openAdjacentSpots(obstacles: Set[Point]): List[Point] = adjacentSpots.filterNot(obstacles.contains)

    def distanceFrom(other: Point, obstacles: Set[Point]): Option[Int] = {
      def distanceFromHelper(queue: Queue[Point],
                             previous: Map[Point, Point],
                             visited: Set[Point]): Option[Int] = {
        if (queue.isEmpty) None
        else {
          val (current, updatedQueue) = queue.dequeue
          if (current == other) {
            val iterator = Iterator.iterate(current)(previous.getOrElse(_, null))
            Some(iterator.takeWhile(_ != null).size - 1)
          } else {
            val newSpots = current.openAdjacentSpots(obstacles)
              .filterNot(visited.contains)
            val previousAdditions = newSpots.map(_ -> current).toMap
            distanceFromHelper(updatedQueue.enqueue(newSpots), previous ++ previousAdditions, visited ++ newSpots.toSet)
          }
        }
      }

      distanceFromHelper(Queue(this), Map(), Set(this))
    }
  }

  case class CombatUnit(id: Point, coordinate: Point, isElf: Boolean, hp: Int = 200, attackPower: Int = 3) {
    //starting coordinates act as ID for the unit

    def canAttack(other: CombatUnit): Boolean = coordinate.adjacentSpots.contains(other.coordinate)

    def canAttack(enemies: List[CombatUnit]): Boolean = unitToAttack(enemies).isDefined

    def unitToAttack(enemies: List[CombatUnit]): Option[CombatUnit] = {
      enemies
        .filter(e => coordinate.adjacentSpots.contains(e.coordinate))
        .sortBy(a => (a.hp, a.coordinate.y, a.coordinate.x))
        .headOption
    }

    def moveTowards(target: Point, obstacles: Set[Point]): CombatUnit = {
      coordinate.openAdjacentSpots(obstacles)
        .sortBy(a => (a.distanceFrom(target, obstacles).getOrElse(Integer.MAX_VALUE), a.y, a.x))
        .headOption
        .map(p => copy(coordinate = p))
        .getOrElse(this)
    }

    def takeHit(attacker: CombatUnit): CombatUnit = copy(hp = hp - attacker.attackPower)

    def isDead: Boolean = hp < 1

    def isAlive: Boolean = hp > 0
  }

  case class CombatArena(units: List[CombatUnit],
                         walls: Set[Point],
                         xRange: Range,
                         yRange: Range,
                         roundFinished: Boolean = true) {
    lazy val allLiveUnits: List[CombatUnit] = units.filterNot(_.isDead)
    lazy val obstacles: Set[Point] = allLiveUnits.map(_.coordinate).toSet ++ walls

    def enemies(unit: CombatUnit): List[CombatUnit] = allLiveUnits.filter(_.isElf != unit.isElf)

    def simulateRound: CombatArena = {
      //      printArena()
      val allUnitsInOrder = allLiveUnits.sortBy(u => (u.coordinate.y, u.coordinate.x))
      allUnitsInOrder.foldLeft(this) { (updatedArena, unit) =>
        val updatedUnit = updatedArena.getUnitById(unit.id)
        if (updatedUnit.isDead) updatedArena
        else if (!updatedArena.inProgress) return updatedArena.copy(roundFinished = false)
        else updatedArena.moveIfNeeded(unit.id).attack(unit.id)
      }
    }

    def getUnitById(id: Point): CombatUnit = units.find(_.id == id).get

    def moveIfNeeded(unitId: Point): CombatArena = {
      val unit = getUnitById(unitId)

      def move: CombatArena = {
        enemies(unit)
          .flatMap(_.coordinate.openAdjacentSpots(obstacles))
          .flatMap(target => unit.coordinate.distanceFrom(target, obstacles).map(target -> _))
          .sortBy(a => (a._2, a._1.y, a._1.x))
          .map(_._1)
          .headOption
          .map(target => moveUnit(unit, unit.moveTowards(target, obstacles)))
          .getOrElse(this)
      }

      unit.unitToAttack(enemies(unit))
        .map(_ => this) //in attack range, no move needed
        .getOrElse(move)
    }

    def attack(unitId: Point): CombatArena = {
      val unit = getUnitById(unitId)
      unit.unitToAttack(enemies(unit))
        .map(attackUnit(unit, _))
        .getOrElse(this)
    }

    def attackUnit(attacker: CombatUnit, defender: CombatUnit): CombatArena =
      copy(defender.takeHit(attacker) :: units.filterNot(_.id == defender.id))

    def moveUnit(oldUnit: CombatUnit, newUnit: CombatUnit): CombatArena =
      copy(newUnit :: units.filterNot(_.id == oldUnit.id))

    def inProgress: Boolean = allLiveUnits.exists(_.isElf) && allLiveUnits.exists(!_.isElf)

    def remainingHitPoints: Int = allLiveUnits.map(_.hp).sum

    def elfSweep: Boolean = units.filter(_.isElf).forall(_.isAlive)

    def printArena(): Unit = {
      println("--------------- ROUND -----------------")
      val unitMap = allLiveUnits.map(u => u.coordinate -> u).toMap
      yRange.foreach { y =>
        val (mapLine, hpLine) = xRange.map { x =>
          val unit = unitMap.get(Point(x, y))
          val mapChar = unit.map(u => if (u.isElf) "E" else "G").getOrElse(if (walls.contains(Point(x, y))) "#" else ".")
          val hp = unit.map(u => s"${if (u.isElf) "E" else "G"}(${u.hp})").getOrElse("")
          mapChar -> hp
        }.unzip
        println(s"${mapLine.mkString} ${hpLine.filter(_.nonEmpty).mkString(", ")}")
      }
    }
  }

  def parseInput(lines: List[String]): CombatArena = {
    val xRange = 0 until lines.head.length
    val yRange = lines.indices

    val walls = lines
      .zipWithIndex
      .flatMap { case (line, y) => line.zipWithIndex.collect { case ('#', x) => Point(x, y) } }
      .toSet

    val e = lines
      .zipWithIndex
      .flatMap { case (line, y) => line.zipWithIndex.collect { case ('E', x) => CombatUnit(Point(x, y), Point(x, y), isElf = true) } }

    val g = lines
      .zipWithIndex
      .flatMap { case (line, y) => line.zipWithIndex.collect { case ('G', x) => CombatUnit(Point(x, y), Point(x, y), isElf = false) } }

    CombatArena(e ++ g, walls, xRange, yRange)
  }

  def simulate(initialArena: CombatArena): (Int, CombatArena) = {
    val simulations = Iterator.iterate(initialArena)(_.simulateRound)
      .takeWhile(_.inProgress)
      .toList
    val endingRound = simulations.last.simulateRound
    val rounds = if (endingRound.roundFinished) simulations.size else simulations.size - 1
    val finalHP = endingRound.remainingHitPoints
    (rounds * finalHP) -> endingRound
  }

  def part1(input: List[String]): Int = simulate(parseInput(input))._1

  def part2(input: List[String], attackPower: Int = 3): Int = {
    val initialArena = parseInput(input)
    val upgradedArena = initialArena.copy(
      units = initialArena.units.map {
        case e if e.isElf => e.copy(attackPower = attackPower)
        case goblin => goblin
      }
    )
    val (result, finalArena) = simulate(upgradedArena)
    if (finalArena.elfSweep) result else part2(input, attackPower + 1)
  }
}