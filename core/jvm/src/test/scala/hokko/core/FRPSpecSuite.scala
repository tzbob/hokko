package hokko.core

import org.scalatest.prop.Checkers
import org.scalatest.{FunSpec, Matchers}

import scala.collection.mutable.ListBuffer
import scala.language.{existentials, reflectiveCalls}

trait FRPSpecSuite extends FunSpec with FRPSuite with Checkers with Matchers

trait FRPSuite {
  def mkOccurrencesWithDependencies[A](ev: Event[A])(primitives: Primitive[_]*)(
      performSideEffects: Engine => Unit): List[A] = {
    val engine      = Engine.compile(ev :: primitives.toList)
    val occurrences = ListBuffer.empty[A]
    val subscription = engine.subscribeForPulses {
      _(ev).foreach(occurrences += _)
    }
    performSideEffects(engine)
    subscription.cancel()
    occurrences.toList
  }

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
