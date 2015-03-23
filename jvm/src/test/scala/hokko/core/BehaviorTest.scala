package hokko.core

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._

class BehaviorTest extends FRPTestSuite {
  describe("Behaviors") {
    describe("that are constant") {
      val const = Behavior.constant(5)
      it("should always return the same value and its changes should never have occurrences") {
        val engine = Engine.compile(Seq.empty, Seq(const))
        val currentValues = engine.askCurrentValues()
        assert(currentValues(const).get === 5)
      }
    }

    describe("that are made from polling functions") {
      it("should represent the current state of the function") {
        var i = 0
        val beh = Behavior.fromPoll(() => i)
        val engine = Engine.compile(Seq.empty, Seq(beh))
        check { (int: Int) =>
          i = int
          val values = engine.askCurrentValues()
          values(beh).get == int
        }
      }
    }

    describe("that are reverse applied ") {
      var param = 0
      var f = (i: Int) => i + 0

      val bParam = Behavior.fromPoll(() => param)
      val bFun = Behavior.fromPoll(() => f)

      it("to changing functions should simply apply the functon") {
        val bApplied = bParam.reverseApply(bFun)
        val engine = Engine.compile(Seq.empty, Seq(bApplied))
        check { (int: Int) =>
          param = int
          f = (i: Int) => i + int
          val currentValues = engine.askCurrentValues()
          currentValues(bApplied).get == int * 2
        }
      }
    }

    describe("that are snapshotted") {
      var param = 0
      val bParam = Behavior.fromPoll(() => param)
      val src = Event.source[Int => Int]
      val snapped = bParam.snapshotWith(src)

      it("produce events that apply the function to the behavior's current value") {
        check { (ints: List[Int]) =>
          val occs = mkOccurrences(snapped) { implicit engine =>
            ints.foreach { i =>
              val f = (x: Int) => i + x
              param = i
              engine.fire(List(src -> f))
            }
          }
          occs == ints.map(_ * 2)
        }
      }
    }
  }
}
