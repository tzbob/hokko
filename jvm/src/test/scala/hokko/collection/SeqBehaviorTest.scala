package hokko.collection

import hokko.core.{ Engine, Event, FRPTestSuite }
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._

class SeqBehaviorTest extends FRPTestSuite {

  describe("SeqBehavior") {
    val src = Event.source[Int]

    describe("that expands from add events") {
      val emptySeq = SeqBehavior.empty[Int]
      val addedSeqs = src +: emptySeq

      it("should stay the same when no events occur") {
        val engine = Engine.compile()(addedSeqs)
        assert(engine.askCurrentValues()(addedSeqs).get === Seq())
      }

      it("should build up all occurrences in cons") {
        check { (ints: List[Int]) =>
          val engine = Engine.compile()(addedSeqs)
          fireAll(src, ints)(engine)
          engine.askCurrentValues()(addedSeqs).get === ints.reverse.toSeq
        }
      }

      it("should build up all occurrences in snoc") {
        check { (ints: List[Int]) =>
          val addedSeqs = emptySeq :+ src
          val engine = Engine.compile()(addedSeqs)
          fireAll(src, ints)(engine)
          engine.askCurrentValues()(addedSeqs).get === ints.toSeq
        }
      }
    }

    val default = (1 to 20).toSeq
    val filledSeq = SeqBehavior.fromSeq(default)
    val addedSeqs = src +: filledSeq :+ src

    describe("that is mapped incrementally") {
      val mappedSeq = addedSeqs.incrementalMap(_.toString)

      it("should map all values accordingly") {
        check { (ints: List[Int]) =>
          val engine = Engine.compile()(mappedSeq)
          fireAll(src, ints)(engine)
          engine.askCurrentValues()(mappedSeq).get === (ints.reverse ++ default ++ ints).map(_.toString)
        }
      }
    }
  }
}
