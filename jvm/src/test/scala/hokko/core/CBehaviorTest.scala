package hokko.core

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._

import hokko.syntax.snapshottable$._

class CBehaviorTest extends FRPTestSuite {
  describe("Behaviors") {
    describe("that are constant") {
      val const = CBehavior.constant(5)
      it("should always return the same value and its changes should never have occurrences") {
        val engine        = Engine.compile(const)
        val currentValues = engine.askCurrentValues()
        assert(currentValues(const).get === 5)
      }
    }

    describe("that are made from polling functions") {
      it("should represent the current state of the function") {
        var i      = 0
        val beh    = CBehavior.fromPoll(() => i)
        val engine = Engine.compile(beh)
        check { (int: Int) =>
          i = int
          val values = engine.askCurrentValues()
          values(beh).get == int
        }
      }
    }

    describe("that are reverse applied ") {
      var param = 0
      var f     = (i: Int) => i + 0

      val bParam = CBehavior.fromPoll(() => param)
      val bFun   = CBehavior.fromPoll(() => f)

      it("to changing functions should simply apply the function") {
        val bApplied = bFun ap bParam
        val engine   = Engine.compile(bApplied)
        check { (int: Int) =>
          param = int
          f = (i: Int) => i + int
          val currentValues = engine.askCurrentValues()
          currentValues(bApplied).get == int * 2
        }
      }
    }

    describe("that are snapshotted") {
      var param   = 0
      val bParam  = CBehavior.fromPoll(() => param)
      val src     = Event.source[Int]
      val snapped = bParam.snapshotWith(src.toEvent)(_ + _)

      it("produce events that apply the function to the behavior's current value") {
        check { (ints: List[Int]) =>
          val occs = mkOccurrences(snapped) { implicit engine =>
            ints.foreach { i =>
              param = i
              engine.fire(List(src -> i))
            }
          }
          occs == ints.map(_ * 2)
        }
      }
    }
  }
}
