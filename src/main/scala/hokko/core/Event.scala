package hokko.core

import scalaz.{ Functor, Need }
import scalaz.std.option._
import scalaz.syntax.applicative._

trait Event[A] {
  private[hokko] val node: Push[A]

  // core

  def fold[B](initial: B)(f: (B, A) => B): DiscreteBehavior[B] =
    DiscreteBehavior.folded(this, initial, f)

  def unionWith[B, C](b: Event[B], f1: A => C, f2: B => C, f3: (A, B) => C): Event[C] =
    Event.fromNode(Event.UnionWith(this, b, f1, f2, f3))

  def collect[B](fb: A => Option[B]): Event[B] =
    Event.fromNode(Event.Collect(this, fb))

  // derived functions

  def map[B](f: A => B): Event[B] =
    collect { (a: A) =>
      Some(f(a))
    }

  def dropIf[B](f: A => Boolean): Event[A] =
    collect { (a: A) =>
      if (f(a)) None else Some(a)
    }
}

sealed trait EventSource[A] extends Event[A]

object Event {
  private[core] def fromNode[A](n: Push[A]): Event[A] =
    new Event[A] { val node = n }

  def empty[A]: Event[A] = fromNode(new NeverPush[A]())
  def source[A]: EventSource[A] = new EventSource[A] { val node = empty[A].node }

  // primitive node implementations

  private[core] def snapshotted[A, B](ev: Event[A => B], snappee: Behavior[A]): Event[B] =
    fromNode(SnapshotWith(ev, snappee))

  // This can't be a case class, we're relying on pointer eq to manually place
  // values of different types with different instances of `NeverPush`
  private class NeverPush[A]() extends Push[A] {
    val dependencies = List.empty
    def pulse(context: TickContext): Option[A] = None
  }

  private case class SnapshotWith[A, B](
    ev: Event[A => B],
    b: Behavior[A]
  ) extends Push[B] {
    val dependencies = List(ev.node, b.node)
    def pulse(context: TickContext): Option[B] =
      for {
        f <- context.getPulse(ev.node)
        thunk <- context.getThunk(b.node)
      } yield f(thunk.value)
  }

  private case class UnionWith[A, B, C](
    evA: Event[A],
    evB: Event[B],
    f1: A => C,
    f2: B => C,
    f3: (A, B) => C
  ) extends Push[C] {
    val dependencies = List(evA.node, evB.node)

    def pulse(context: TickContext): Option[C] = {
      val aPulse = context.getPulse(evA.node)
      val bPulse = context.getPulse(evB.node)

      (aPulse, bPulse) match {
        case (Some(a), None) => Some(f1(a))
        case (None, Some(b)) => Some(f2(b))
        case (Some(a), Some(b)) => Some(f3(a, b))
        case _ => None
      }
    }
  }

  private case class Collect[A, B](
    ev: Event[A],
    f: A => Option[B]
  ) extends Push[B] {
    val dependencies = List(ev.node)
    def pulse(context: TickContext): Option[B] =
      context.getPulse(ev.node).flatMap(f(_))
  }

  implicit val evtFunctor = new Functor[Event] {
    def map[A, B](fa: Event[A])(f: A => B): Event[B] =
      fa.map(f)
  }
}
