package hokko.core

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.scalatest.prop.Checkers
import scala.collection.mutable.ListBuffer
import scala.language.{ existentials, reflectiveCalls }
import scalaz.syntax.applicative._

class EventTest extends FunSpec with Checkers with Matchers {
  implicit override val generatorDrivenConfig = PropertyCheckConfig(minSize = 10)

  def mkOccurrences[A](ev: Event[A])(performSideEffects: Engine => Unit): List[A] = {
    val engine = Engine.compile(ev)()
    val occurrences = ListBuffer.empty[A]
    val subscription = engine.subscribeForPulses {
      _(ev).foreach(occurrences += _)
    }
    performSideEffects(engine)
    subscription.cancel()
    occurrences.toList
  }

  describe("Events") {
    describe("that are folded") {
      val src = Event.source[Int]
      it("should have a current value equal to " +
        "the initial value when the source event has no occurrences") {
        check { (i: Int) =>
          val beh = src.fold(i)(_ + _)
          val engine = Engine.compile()(beh)
          val currentValues = engine.askCurrentValues()
          currentValues(beh).get.value == i
        }
      }
      it("should have a current value representing the total accumulation of occurrences") {
        val beh = src.fold(0)(_ + _)
        check { (ints: List[Int]) =>
          val engine = Engine.compile()(beh)
          ints.foreach(i => engine.fire(src -> i))
          val currentValues = engine.askCurrentValues()
          currentValues(beh).get.value == ints.sum
        }
      }
    }
    describe("that unify other events using behaviors f1, f2 and f3") {
      val src1 = Event.source[Int]
      val src2 = Event.source[Double]

      val f1: Int => String = _.toString
      val f2: Double => String = _.toString
      val f3: (Int, Double) => String = (_, _).toString

      val union = src1.unionWith(src2, f1, f2, f3)

      it("should have occurrences matching f1 when left dependency fires") {
        check { (ints: List[Int]) =>
          val occurrences = mkOccurrences(union) { engine =>
            ints.foreach { i =>
              engine.fire(src1 -> i)
            }
          }
          occurrences == ints.map(_.toString)
        }
      }

      it("should have occurrences matching f2 when right dependency fires") {
        check { (doubles: List[Double]) =>
          val occurrences = mkOccurrences(union) { engine =>
            doubles.foreach { d =>
              engine.fire(src2 -> d)
            }
          }
          occurrences == doubles.map(_.toString)
        }
      }

      it("should have occurrences matching f3 when both dependencies fire") {
        check { (intDoubles: List[(Int, Double)]) =>
          val occurrences = mkOccurrences(union) { engine =>
            intDoubles.foreach {
              case (i, d) => engine.fire(src1 -> i, src2 -> d)
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
        val collected = src.collect(_ => None)

        check { (ints: List[Int]) =>
          val occurrences = mkOccurrences(collected) { engine =>
            ints.foreach(i => engine.fire(src -> i))
          }
          occurrences == List.empty
        }
      }

      it("should have all occurrences doubled when collecting everything doubled") {
        val collected = src.collect(i => Some(i * 2))

        check { (ints: List[Int]) =>
          val occurrences = mkOccurrences(collected) { engine =>
            ints.foreach(i => engine.fire(src -> i))
          }
          occurrences == ints.map(_ * 2)
        }
      }

      it("should only have even occurrences when collecting even values") {
        def even(v: Int): Boolean = v % 2 == 0
        val collected = src.collect { i =>
          if (even(i)) Some(i)
          else None
        }

        check { (ints: List[Int]) =>
          val occurrences = mkOccurrences(collected) { engine =>
            ints.foreach(i => engine.fire(src -> i))
          }
          occurrences == ints.filter(even)
        }
      }
    }
  }
}
