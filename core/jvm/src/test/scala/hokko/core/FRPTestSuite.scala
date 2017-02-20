package hokko.core

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalatest.{FunSpec, Matchers}
import org.scalatest.prop.Checkers

import scala.collection.immutable.Seq
import scala.collection.mutable.ListBuffer
import scala.language.{existentials, reflectiveCalls}

trait FRPTestSuite extends FunSpec with Checkers with Matchers {
  def mkOccurrences[A](ev: Event[A])(
      performSideEffects: Engine => Unit): List[A] = {
    val engine      = Engine.compile(ev)
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

  def mkOccurrencesWithPulses[A, B](
      target: Event[B])(src: EventSource[A], pulses: List[A]): List[B] =
    mkOccurrences(target) { implicit engine =>
      fireAll(src, pulses)
    }
}