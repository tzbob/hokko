package hokko.core

class IBehaviorTest extends FRPTestSuite {
  describe("IBehaviors") {
    describe("that are constant") {
      val const = IBehavior.constant(5)
      it("should always return the same value and its changes should never have occurrences") {
        val occurrences = mkOccurrences(const.changes) { implicit engine =>
          val currentValues = engine.askCurrentValues()
          assert(currentValues(const.toCBehavior).get === 5)
        }
        assert(occurrences === List.empty)
      }
    }

    describe("that are incrementally mapped") {
      val src                         = Event.source[Int]
      val bParam: IBehavior[Int, Int] = src.toEvent.fold(0)(_ + _)

      it("to constant functions should simply apply the functon and have its results on .changes") {
        val incrementalTimesTwo = bParam.incMap(_ * 2)(_ * 2)(_ + _)

        check { (ints: List[Int]) =>
          val doubleInts = ints.map(_ * 2)
          val occs = mkOccurrences(incrementalTimesTwo.changes) {
            implicit engine =>
              fireAll(src, ints)
              val currentValues = engine.askCurrentValues()
              currentValues(incrementalTimesTwo.toCBehavior).get == doubleInts.sum
          }

          occs == doubleInts.scanLeft(0)(_ + _).tail
        }
      }
    }

  }
}
