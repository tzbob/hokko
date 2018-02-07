package hokko.core

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

class IBehaviorTest extends FunSuite with FRPSuite with Checkers {

  test("Constant Behaviors return constants and don't have occurences") {
    val const = IBehavior.constant(5)
    val occurrences = mkOccurrences(const.changes) { implicit engine =>
      val currentValues = engine.askCurrentValues()
      assert(currentValues(const.toCBehavior).get === 5)
    }
    assert(occurrences === List.empty)
  }

  test("IBehaviors incrementally map to others") {
    val src                         = Event.source[Int]
    val bParam: IBehavior[Int, Int] = src.fold(0)(_ + _)

    val incrementalTimesTwo = bParam.incMap(_ * 2)(_ * 2)(_ + _)

    check { (ints: List[Int]) =>
      val doubleInts = ints.map(_ * 2)
      val occs = mkOccurrences(incrementalTimesTwo.changes) { implicit engine =>
        fireAll(src, ints)
        val currentValues = engine.askCurrentValues()
        currentValues(incrementalTimesTwo.toCBehavior).get == doubleInts.sum
      }

      occs == doubleInts.scanLeft(0)(_ + _).tail
    }
  }

  // FIXME
  test("IBehaviors incrementally map 2") {}

  test("IBehaviors can be reset without producing pulses") {
    val resetter = Event.source[Int]
    val const    = IBehavior.constant(5)
    val resetted = const.resetState(resetter)

    check { (ints: List[Int]) =>
      val cBehavior = resetted.toCBehavior

      val occs =
        mkOccurrencesWithDependencies(resetted.changes)(cBehavior) {
          implicit
          engine =>
            fireAll(resetter, ints)
            val currentValues = engine.askCurrentValues()
            val value         = currentValues(cBehavior)
            println(value)
            value.get === (5 :: ints).last
        }

      occs === List()
    }
  }
}
