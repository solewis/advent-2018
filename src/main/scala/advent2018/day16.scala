package advent2018

import advent2018.day16.Operations.OpFunction

object day16 extends App {

  object Operations {
    type Op = (Vector[Int], Int, Int) => Int
    type OpFunction = (Vector[Int], Int, Int, Int) => Vector[Int]

    val addr: Op = (registers: Vector[Int], a: Int, b: Int) => registers(a) + registers(b)
    val addi: Op = (registers: Vector[Int], a: Int, b: Int) => registers(a) + b
    val mulr: Op = (registers: Vector[Int], a: Int, b: Int) => registers(a) * registers(b)
    val muli: Op = (registers: Vector[Int], a: Int, b: Int) => registers(a) * b
    val banr: Op = (registers: Vector[Int], a: Int, b: Int) => registers(a) & registers(b)
    val bani: Op = (registers: Vector[Int], a: Int, b: Int) => registers(a) & b
    val borr: Op = (registers: Vector[Int], a: Int, b: Int) => registers(a) | registers(b)
    val bori: Op = (registers: Vector[Int], a: Int, b: Int) => registers(a) | b
    val setr: Op = (registers: Vector[Int], a: Int, b: Int) => registers(a)
    val seti: Op = (registers: Vector[Int], a: Int, b: Int) => a
    val gtir: Op = (registers: Vector[Int], a: Int, b: Int) => if (a > registers(b)) 1 else 0
    val gtri: Op = (registers: Vector[Int], a: Int, b: Int) => if (registers(a) > b) 1 else 0
    val gtrr: Op = (registers: Vector[Int], a: Int, b: Int) => if (registers(a) > registers(b)) 1 else 0
    val eqir: Op = (registers: Vector[Int], a: Int, b: Int) => if (a == registers(b)) 1 else 0
    val eqri: Op = (registers: Vector[Int], a: Int, b: Int) => if (registers(a) == b) 1 else 0
    val eqrr: Op = (registers: Vector[Int], a: Int, b: Int) => if (registers(a) == registers(b)) 1 else 0

    val toOpFunction: Op => OpFunction = (op: Op) =>
      (registers: Vector[Int], a: Int, b: Int, c: Int) => registers.patch(c, Seq(op(registers, a, b)), 1)

    val allOperations: List[OpFunction] =
      List(addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr)
        .map(toOpFunction)
  }

  case class Sample(before: Vector[Int], after: Vector[Int], instructions: Vector[Int]) {
    lazy val opCode = instructions(0)
    lazy val valueOfA = instructions(1)
    lazy val valueOfB = instructions(2)
    lazy val valueOfC = instructions(3)

    def matchesOperation(op: OpFunction): Boolean = op(before, valueOfA, valueOfB, valueOfC) == after

    def possibleOpcodesCount(ops: List[OpFunction]): Int =
      ops.count(matchesOperation)

    def possibleOpcodes(ops: List[OpFunction]): List[OpFunction] =
      ops.filter(matchesOperation)
  }

  def parseSamples(input: List[String]): List[Sample] = {
    val beforeRegex = """Before: \[(\d), (\d), (\d), (\d)\]""".r
    val instructionsRegex = """(\d+) (\d) (\d) (\d)""".r
    val afterRegex = """After:  \[(\d), (\d), (\d), (\d)\]""".r
    input
      .filterNot(_.isEmpty)
      .grouped(3)
      .map { case List(beforeRegex(ba, bb, bc, bd), instructionsRegex(ia, ib, ic, id), afterRegex(aa, ab, ac, ad)) =>
        Sample(
          Vector(ba, bb, bc, bd).map(_.toInt),
          Vector(aa, ab, ac, ad).map(_.toInt),
          Vector(ia, ib, ic, id).map(_.toInt)
        )
      }.toList
  }

  def part1(input: List[String]): Int = {
    parseSamples(input)
      .count(_.possibleOpcodesCount(Operations.allOperations) > 2)
  }

  def resolveOpcodes(samples: List[Sample]): Map[Int, OpFunction] = {
    def possibleOpFunctions(samples: List[Sample]) =
      samples.map(_.possibleOpcodes(Operations.allOperations))

    //Only the functions that are found in all samples of a given code are possibilities
    def ubiquitousOpFunctions(fns: List[List[OpFunction]]) =
      fns.flatten.distinct.filter(f => fns.forall(_.contains(f)))

    val opcodePossibilities: Map[Int, List[OpFunction]] = samples
      .groupBy(_.opCode)
      .mapValues(possibleOpFunctions)
      .mapValues(ubiquitousOpFunctions)

    //Take the map of opcode to List of possible functions and narrow it down to one function per opcode.
    def refine(current: Map[Int, List[OpFunction]]): Map[Int, OpFunction] = {
      if (current.forall(_._2.size == 1)) current.mapValues(_.head)
      else {
        // Find the opcodes that are already determined and remove them as possibilities from the remaining codes
        val determined = current.filter(_._2.size == 1).map(_._2.head).toSet
        val updated = current
          .mapValues(v => if (v.size == 1) v else v.filterNot(determined.contains))
        refine(updated)
      }
    }

    refine(opcodePossibilities)
  }

  def part2(samplesInput: List[String], instructionsInput: List[String]): Int = {
    val opcodeMap = resolveOpcodes(parseSamples(samplesInput))
    val instructions = instructionsInput.map(_.split(" ").map(_.toInt)).toVector
    val finalRegisters = instructions.foldLeft(Vector(0, 0, 0, 0)) { case (registers, instruction) =>
      opcodeMap(instruction(0))(registers, instruction(1), instruction(2), instruction(3))
    }
    finalRegisters(0)
  }
}
