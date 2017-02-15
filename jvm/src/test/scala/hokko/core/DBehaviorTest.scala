package hokko.core

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._

import cats.syntax.all._

class DBehaviorTest extends FRPTestSuite {
  describe("DBehaviors") {
    describe("that are constant") {
      val const = DBehavior.constant(5)
      it("should always return the same value and its changes should never have occurrences") {
        val occurrences = mkOccurrences(const.changes) { implicit engine =>
          val currentValues = engine.askCurrentValues()
          assert(currentValues(const.toCBehavior).get === 5)
        }
        assert(occurrences === List.empty)
      }
    }
    describe("that are reverse applied ") {
      val src = Event.source[Int]
      val bParam: DBehavior[Int] = src
        .fold(0) { (acc, n) =>
          n
        }
        .toDBehavior

      it("to constant functions should simply apply the functon and have its results on .changes") {
        val const    = DBehavior.constant((_: Int) * 2)
        val bApplied = const ap bParam
        check { (ints: List[Int]) =>
          val occs = mkOccurrences(bApplied.changes) { implicit engine =>
            fireAll(src, ints)
            val currentValues = engine.askCurrentValues()
            currentValues(bApplied.toCBehavior).get == ints.lastOption
              .map(_ * 2)
              .getOrElse(0)
          }
          occs == ints.map(_ * 2)
        }
      }

      it("to changing functions should simply apply the functon and have its results on .changes") {
        val bPoorMansDouble = bParam.map { (i: Int) => (int: Int) =>
          int + i
        }
        val bApplied = bPoorMansDouble ap bParam
        check { (ints: List[Int]) =>
          val occs = mkOccurrences(bApplied.changes) { implicit engine =>
            fireAll(src, ints)
            val currentValues = engine.askCurrentValues()
            currentValues(bApplied.toCBehavior).get == ints.lastOption
              .map(_ * 2)
              .getOrElse(0)
          }
          occs == ints.map(_ * 2)
        }
      }

      it("should be convertable to ibehaviors") {
        val ib = bParam.toIBehavior(_ - _)(_ + _)
        check { (ints: List[Int]) =>
          val occurrences = mkOccurrencesWithPulses(ib.deltas)(src, ints)
          val expected = (0 +: ints, (0 +: ints).tail).zipped.map { (o, n) =>
            n - o
          }
          occurrences == expected
        }
      }
    }
  }
}
