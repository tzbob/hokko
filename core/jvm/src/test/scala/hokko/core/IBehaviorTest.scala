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
    val bParam: IBehavior[Int, Int] = src.foldI(0)(_ + _)

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
    val count: IBehavior[Int, Int] = src.foldI(0) { (a, b) =>
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

            def currentValue() = engine.askCurrentValues()(cBehavior)
            assert(currentValue().get === initAfterReset)

            // Tests if fold starts from reset value
            fireAll(source, ints)
            assert(currentValue().get === totalSum)
        }

      val changes = ints.scan(initAfterReset)(_ + _).drop(1)
      occs === changes
    }
  }

  test("Behaviors dependent on reset IBehaviors change properly") {
    check { (ints: List[Int]) =>
      val init     = 5
      val source   = Event.source[Int]
      val resetter = Event.source[Int]
      val reset    = source.resetFold(resetter)(init)(_ + _)

      val plus10   = reset.toDBehavior.map(_ + 10)
      val plus10cb = plus10.toCBehavior

      // start from 10 (reset) and apply _ + 10
      val totalSum = (10 :: ints).sum + 10

      val occs = mkOccurrencesWithDependencies(plus10.changes(

      ))(plus10cb) {
        implicit
        engine =>
          // reset to 10
          fireAll(resetter, List(10))
          def currentValue() = engine.askCurrentValues()(plus10cb)
          // check if the plus10 value starts from 10 + 10
          assert(currentValue().get === 10 + 10)

          // Tests if fold starts from reset value
          fireAll(source, ints)
          assert(currentValue().get === totalSum)
      }

      occs === ints.scan(10)(_ + _).drop(1).map(_ + 10)
    }
  }

}
