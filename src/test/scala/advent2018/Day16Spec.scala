package advent2018

import advent2018.day16.Sample
import advent2018.day16.Operations._
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class Day16Spec extends FlatSpec with Matchers {

  val sample = Sample(Vector(3, 2, 1, 1), Vector(3, 2, 2, 1), Vector(9, 2, 1, 2))
  val op: Op => Vector[Int] = (op: Op) => toOpFunction(op)(sample.before, sample.valueOfA, sample.valueOfB, sample.valueOfC)

  "addr" should "store result of adding register A and register B" in {
    op(addr) should be(Vector(3, 2, 3, 1))
  }

  "addi" should "store result of adding register A and value B" in {
    op(addi) should be(sample.after)
  }

  "mulr" should "store result of multiplying register A and register B" in {
    op(mulr) should be(sample.after)
  }

  "muli" should "store result of multiplying register A and value B" in {
    op(muli) should be(Vector(3, 2, 1, 1))
  }

  "banr" should "store result of bitwise AND of register A and register B" in {
    op(banr) should be(Vector(3, 2, 0, 1))
  }

  "bani" should "store result of bitwise AND of register A and value B" in {
    op(bani) should be(Vector(3, 2, 1, 1))
  }

  "borr" should "store result of bitwise OR of register A and register B" in {
    op(borr) should be(Vector(3, 2, 3, 1))
  }

  "bori" should "store result of bitwise OR of register A and value B" in {
    op(bori) should be(Vector(3, 2, 1, 1))
  }

  "setr" should "store register a" in {
    op(setr) should be(Vector(3, 2, 1, 1))
  }

  "seti" should "store value a" in {
    op(seti) should be(sample.after)
  }

  "gtir" should "store 1 if value A is greater than register B. Otherwise 0" in {
    op(gtir) should be (Vector(3, 2, 0, 1))
  }

  "gtri" should "store 1 if register A is greater than value B. Otherwise 0" in {
    op(gtri) should be (Vector(3, 2, 0, 1))
  }

  "gtrr" should "store 1 if register A is greater than register B. Otherwise 0" in {
    op(gtrr) should be (Vector(3, 2, 0, 1))
  }

  "eqir" should "store 1 if value A is equal to register B. Otherwise 0" in {
    op(eqir) should be (Vector(3, 2, 1, 1))
  }

  "eqri" should "store 1 if register A is equal to value B. Otherwise 0" in {
    op(eqri) should be (Vector(3, 2, 1, 1))
  }

  "eqrr" should "store 1 if register A is equal to register B. Otherwise 0" in {
    op(eqrr) should be (Vector(3, 2, 0, 1))
  }

  "single sample with 3 possibilities" should "return a count of 1" in {
    val samples =
      """
        |Before: [3, 2, 1, 1]
        |9 2 1 2
        |
        |After:  [3, 2, 2, 1]
      """.stripMargin.trim
    val input = Source.fromString(samples).getLines().toList
    day16.part1(input) should be(1)
  }

  "part1" should "return the number of samples that have more than 2 possibilities" in {
    val input = Source.fromResource("day16-samples.txt").getLines().toList
    day16.part1(input) should be (500)
  }

  "part2" should "" in {
    val samples = Source.fromResource("day16-samples.txt").getLines().toList
    val instructions = Source.fromResource("day16-instructions.txt").getLines().toList
    day16.part2(samples, instructions) should be (533)
  }
}
