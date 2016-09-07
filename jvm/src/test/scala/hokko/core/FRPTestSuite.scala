package hokko.core

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalatest.FunSpec
import org.scalatest.prop.Checkers
import scala.collection.mutable.ListBuffer
import scala.language.{existentials, reflectiveCalls}

trait FRPTestSuite extends FunSpec with Checkers {
  def mkOccurrences[A](ev: Event[A])(
      performSideEffects: Engine => Unit): List[A] = {
    val engine      = Engine.compile(Seq(ev), Seq.empty)
    val occurrences = ListBuffer.empty[A]
    val subscription = engine.subscribeForPulses {
      _(ev).foreach(occurrences += _)
    }
    performSideEffects(engine)
    subscription.cancel()
    occurrences.toList
  }

  def fireAll[A](src: EventSource[A], pulses: List[A])(
      implicit engine: Engine) =
    pulses.foreach(i => engine.fire(List(src -> i)))

  def mkOccurrencesWithPulses[A, B](target: Event[B])(src: EventSource[A],
                                                      pulses: List[A]) =
    mkOccurrences(target) { implicit engine =>
      fireAll(src, pulses)
    }
}
