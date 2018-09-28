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

  test("IBehaviors can be turned into continuous behaviors") {
    var counter = 0

    val src = Event.source[Int]
    val count: IBehavior[Int, Int] = src.fold(0) { (a, b) =>
      counter += 1
      a + b
    }
    val countCB      = count.toCBehavior
    val countChanges = count.changes

    check { (ints: List[Int]) =>
      counter = 0
      val occs = mkOccurrencesWithDependencies(src)(countCB) {
        implicit engine =>
          fireAll(src, ints)
          engine.askCurrentValues()(countCB).get == ints.sum

          engine.askCurrentValues()(countCB).get == ints.sum
      }

      val isFoldExecutedOnce = counter == ints.size
      occs == ints && isFoldExecutedOnce
    }

  }

  test("IBehaviors can be reset without producing pulses") {
    val init = 5

    val source   = Event.source[Int]
    val resetter = Event.source[Int]
    val reset    = source.resetFold(resetter)(init)(_ + _)

    check { (ints: List[Int]) =>
      val cBehavior = reset.toCBehavior

      val initAfterReset = (init :: ints).last
      val totalSum       = (initAfterReset :: ints).sum

      val occs =
        mkOccurrencesWithDependencies(reset.changes)(cBehavior) {
          implicit
          engine =>
            // Tests resetting value
            fireAll(resetter, ints)
            val currentValues = engine.askCurrentValues()
            val resetResult   = currentValues(cBehavior)

            val isValueReset = resetResult.get === initAfterReset

            // Tests if fold starts from reset value
            fireAll(source, ints)
            val foldAfterResetResult = engine.askCurrentValues()(cBehavior)

            val doesFoldStartFromReset = resetResult.get === totalSum

            isValueReset && doesFoldStartFromReset
        }

      val changes = ints.scan(initAfterReset)(_ + _).drop(1)
      occs === changes
    }
  }
}
