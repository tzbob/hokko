package hokko.core

import hokko.syntax.EventSyntax
import scala.scalajs.js.annotation.JSExport

trait Event[+A] {
  private[core] val node: Push[A]

  @JSExport
  def fold[B, AA >: A](initial: B)(f: (B, AA) => B): IncrementalBehavior[B, AA] =
    IncrementalBehavior.folded(this, initial, f)

  @JSExport
  def unionWith[B, C, AA >: A](b: Event[B])(f1: AA => C)(f2: B => C)(f3: (AA, B) => C): Event[C] =
    Event.fromNode(Event.UnionWith(this, b, f1, f2, f3))

  @JSExport
  def collect[B, AA >: A](fb: A => Option[B]): Event[B] =
    Event.fromNode(Event.Collect(this, fb))
}

sealed trait EventSource[+A] extends Event[A]

object Event extends EventSyntax {
  private[core] def fromNode[A](n: Push[A]): Event[A] =
    new Event[A] { val node = n }

  @JSExport
  def empty[A]: Event[A] = fromNode(new NeverPush[A]())

  @JSExport
  def source[A]: EventSource[A] = new EventSource[A] { val node = empty[A].node }

  // primitive node implementations

  private[core] def snapshotted[A, B](ev: Event[A => B], snappee: Behavior[A]): Event[B] =
    fromNode(SnapshotWith(ev, snappee))

  // This can't be a case class, we're relying on reference eq to manually place
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
      } yield f(thunk.force)
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
}