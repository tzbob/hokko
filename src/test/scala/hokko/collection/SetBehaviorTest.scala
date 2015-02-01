package hokko.collection

import hokko.core.Engine
import hokko.core.Event
import hokko.core.FRPTestSuite
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._

class SetBehaviorTest extends FRPTestSuite {
  describe("SetBehavior") {
    val src = Event.source[Int]

    describe("that expands from add events") {
      val emptySet = SetBehavior.empty[Int]
      val addedSets = emptySet + src

      it("should stay the same when no events occur") {
        val engine = Engine.compile()(addedSets)
        assert(engine.askCurrentValues()(addedSets).get.value === Set())
      }

      it("should build up all occurrences") {
        check { (ints: List[Int]) =>
          val engine = Engine.compile()(addedSets)
          fireAll(src, ints)(engine)
          engine.askCurrentValues()(addedSets).get.value === ints.toSet
        }
      }
    }

    val default = (1 to 200).toSet
    val filledSet = SetBehavior.fromSet(default)
    val addedSets = filledSet + src

    describe("that shrinks from delete events") {
      val removedSet = filledSet - src

      it("should remove all occurrences") {
        check { (ints: List[Int]) =>
          val engine = Engine.compile()(removedSet)
          fireAll(src, ints)(engine)
          engine.askCurrentValues()(removedSet).get.value === (default -- ints)
        }
      }
    }

    describe("that is mapped incrementally") {
      val mappedSet = addedSets.incrementalMap(_.toString)

      it("should map all values accordingly") {
        check { (ints: List[Int]) =>
          val engine = Engine.compile()(mappedSet)
          fireAll(src, ints)(engine)
          engine.askCurrentValues()(mappedSet).get.value === (default ++ ints).map(_.toString)
        }
      }
    }
  }
}
