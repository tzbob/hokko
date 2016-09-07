package hokko.core

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalatest.FunSpec
import scala.language.{existentials, reflectiveCalls}

class EventTest extends FRPTestSuite {
  describe("Events") {
    describe("that are folded") {
      val src = Event.source[Int]
      it(
        "should have a current value equal to " +
          "the initial value when the source event has no occurrences") {
        check { (i: Int) =>
          val beh           = src.toEvent.fold(i)(_ + _).toCBehavior
          val engine        = Engine.compile(Seq.empty, Seq(beh))
          val currentValues = engine.askCurrentValues()
          currentValues(beh).get == i
        }
      }

      it("should have a current value representing the total accumulation of occurrences") {
        val beh = src.toEvent.fold(0)(_ + _).toCBehavior
        check { (ints: List[Int]) =>
          val engine = Engine.compile(Seq.empty, Seq(beh))
          fireAll(src, ints)(engine)
          val currentValues = engine.askCurrentValues()
          currentValues(beh).get == ints.sum
        }
      }
    }

    describe("that unify other events using behaviors f1, f2 and f3") {
      val src1 = Event.source[Int]
      val src2 = Event.source[Double]

      val union =
        src1.toEvent.unionWith(src2.toEvent)(_.toString)(_.toString) {
          (_, _).toString
        }

      it("should have occurrences matching f1 when left dependency fires") {
        check { (ints: List[Int]) =>
          val occurrences = mkOccurrencesWithPulses(union)(src1, ints)
          occurrences == ints.map(_.toString)
        }
      }

      it("should have occurrences matching f2 when right dependency fires") {
        check { (doubles: List[Double]) =>
          val occurrences = mkOccurrencesWithPulses(union)(src2, doubles)
          occurrences == doubles.map(_.toString)
        }
      }

      it("should have occurrences matching f3 when both dependencies fire") {
        check { (intDoubles: List[(Int, Double)]) =>
          val occurrences = mkOccurrences(union) { engine =>
            intDoubles.foreach {
              case (i, d) => engine.fire(List(src1 -> i, src2 -> d))
            }
          }
          occurrences == intDoubles.map {
            case (i, d) => (i, d).toString
          }
        }
      }
    }

    describe("that are collected") {
      val src = Event.source[Int]

      it("should have no occurrences when collecting nothing") {
        val collected = src.toEvent.collect(_ => None)

        check { (ints: List[Int]) =>
          val occurrences = mkOccurrencesWithPulses(collected)(src, ints)
          occurrences == List.empty
        }
      }

      it("should have all occurrences doubled when collecting everything doubled") {
        val collected = src.toEvent.collect(i => Some(i * 2))

        check { (ints: List[Int]) =>
          val occurrences = mkOccurrencesWithPulses(collected)(src, ints)
          occurrences == ints.map(_ * 2)
        }
      }

      it("should only have even occurrences when collecting even values") {
        def even(v: Int): Boolean = v % 2 == 0
        val collected = src.toEvent.collect { i =>
          if (even(i)) Some(i)
          else None
        }

        check { (ints: List[Int]) =>
          val occurrences = mkOccurrencesWithPulses(collected)(src, ints)
          occurrences == ints.filter(even)
        }
      }
    }
  }
}
