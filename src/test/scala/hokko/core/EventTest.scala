package hokko.core

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalatest.FunSpec
import org.scalatest.prop.Checkers
import scala.collection.mutable.ListBuffer
import scala.language.{ existentials, reflectiveCalls }
import scalaz.syntax.applicative._

class EventTest extends FunSpec with Checkers {
  implicit override val generatorDrivenConfig = PropertyCheckConfig(minSize = 10)

  // implicitly lift to constants to make testing easier
  implicit def liftBehavior[A](a: A): Behavior[A] = Behavior.constant(a)

  def mkOccurrences[A](ev: Event[A])(performSideEffects: Engine => Unit): List[A] = {
    val engine = Engine.compile(ev)
    val occurrences = ListBuffer.empty[A]
    val subscription = engine.subscribeForEvent(ev) {
      _.foreach(occurrences += _)
    }
    performSideEffects(engine)
    subscription.cancel()
    occurrences.toList
  }

  describe("Events") {
    describe("that unify other events using behaviors f1, f2 and f3") {
      val src1 = Event.source[Int]
      val src2 = Event.source[Double]

      // Set up dummy variables to wrap with pull behaviors
      var f1: Int => String = x => x.toString
      var f2: Double => String = x => x.toString
      var f3: (Int, Double) => String = (i, d) => (i, d).toString

      val b1 = Behavior.byPulling(() => f1)
      val b2 = Behavior.byPulling(() => f2)
      val b3 = Behavior.byPulling(() => f3)

      val union: Event[String] = src1.unionWithBehavior(src2, b1, b2, b3)

      it("should have occurrences matching f1 when left dependency fires") {
        check { (ints: List[Int]) =>
          val occurrences = mkOccurrences(union) { engine =>
            ints.foreach { i =>
              f1 = x => (x + i).toString
              engine.fire(src1 -> i)
            }
          }
          occurrences.toList == ints.map(i => (i * 2).toString)
        }
      }

      it("should have occurrences matching f2 when right dependency fires") {
        check { (doubles: List[Double]) =>
          val occurrences = mkOccurrences(union) { engine =>
            doubles.foreach { d =>
              f2 = x => (x + d).toString
              engine.fire(src2 -> d)
            }
          }
          occurrences.toList == doubles.map(d => (d * 2).toString)
        }
      }

      it("should have occurrences matching f3 when both dependencies fire") {
        check { (intDoubles: List[(Int, Double)]) =>
          val occurrences = mkOccurrences(union) { engine =>
            intDoubles.foreach {
              case (i, d) =>
                f3 = (x, y) => (x + i, y + d).toString
                engine.fire(src1 -> i, src2 -> d)
            }
          }
          occurrences.toList == intDoubles.map {
            case (i, d) => (i * 2, d * 2).toString
          }
        }
      }
    }
  }
}
