package hokko.core

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import cats.syntax.all._
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

class DBehaviorTest extends FunSuite with FRPSuite with Checkers {
  val src = Event.source[Int]
  val intDBehavior: DBehavior[Int] = src
    .fold(0) { (acc, n) =>
      n
    }
    .toDBehavior

  test("Constant DBehaviors return constants and don't have occurences") {
    val const = DBehavior.constant(5)
    val test = const.map { i =>
      i
    }
    val occurrences = mkOccurrences(test.changes) { implicit engine =>
      val currentValues = engine.askCurrentValues()
      assert(currentValues(const.toCBehavior).get === 5)
    }
    assert(occurrences === List.empty)
  }

  test("DBehaviors can be applied") {
    val bPoorMansDouble = intDBehavior.map { (i: Int) => (int: Int) =>
      int + i
    }
    val bApplied = bPoorMansDouble ap intDBehavior
    check { (ints: List[Int]) =>
      val occs = mkOccurrences(bApplied.changes) { implicit engine =>
        fireAll(src, ints)
        val currentValues = engine.askCurrentValues()
        assert(
          currentValues(bApplied.toCBehavior).get === ints.lastOption
            .map(_ * 2)
            .getOrElse(0))
      }
      occs == ints.map(_ * 2)
    }
  }

  test("DBehaviors can be diffed into IBehaviors") {
    val ib = intDBehavior.toIBehavior(_ - _)(_ + _)
    check { (ints: List[Int]) =>
      val occurrences = mkOccurrencesWithPulses(ib.deltas)(src, ints)
      val expected = (0 +: ints, (0 +: ints).tail).zipped.map { (o, n) =>
        n - o
      }
      occurrences == expected
    }
  }

  test("DBehaviors can be defined recursively through delay with objects") {
    object Test {
      val delayedInts: DBehavior[Int] = DBehavior.delayed(player, 100)
      val player                      = intDBehavior.map2(delayedInts)(_ + _)
    }

    val player = Test.player

    def checkForList(ints: List[Int]) = ints.foldLeft(100) { (acc, int) =>
      acc + int
    }

    check { (ints: List[Int]) =>
      val occs = mkOccurrences(player.changes) { implicit engine =>
        val preOccValues = engine.askCurrentValues()
        assert(
          preOccValues(player.toCBehavior).get === checkForList(List.empty))
        assert(player.init === checkForList(List.empty))

        val results = fireAll(src, ints)
        results.lastOption.foreach { last =>
          assert(last.values(player.toCBehavior).get === checkForList(ints))
        }

        val currentValues = engine.askCurrentValues()

        val checkLast = checkForList(ints :+ ints.lastOption.getOrElse(0))
        assert(currentValues(player.toCBehavior).get === checkLast)
      }

      val correctOccs =
        ints.inits.filterNot(_.isEmpty).toList.reverse.map(checkForList)
      occs === correctOccs
    }

  }
}
