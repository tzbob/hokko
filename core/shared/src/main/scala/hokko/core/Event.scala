package hokko.core

import cats.Functor
import cats.syntax.FunctorSyntax
import hokko.syntax.EventSyntax

sealed trait Event[+A] extends Primitive[A] {
  override private[core] val node: Push[A]

  def resetFold[B, AA >: A](resetter: Event[B])(init: B)(
      f: (B, AA) => B): IBehavior[B, AA] =
    IBehavior.resetFolded(this, resetter, init, f)
}

final class EventSource[+A](private[core] val node: Push[A]) extends Event[A]

object EventSource {
  implicit def toFunctorOpsEvtSrc[A](target: EventSource[A])(
      implicit tc: Functor[Event]): Functor.Ops[Event, A] = {
    val evt: Event[A] = target
    Event.toFunctorOps(evt)(tc)
  }
}

object Event
    extends EventSyntax[Event, DBehavior, IBehavior]
    with FunctorSyntax {
  implicit val hokkoEventInstances = new tc.Event[Event, DBehavior, IBehavior] {
    override def fold[A, DeltaA](ev: Event[DeltaA], initial: A)(
        f: (A, DeltaA) => A): DBehavior[A] =
      foldI(ev, initial)(f).toDBehavior

    override def foldI[A, DeltaA](ev: Event[DeltaA], initial: A)(
        f: (A, DeltaA) => A): IBehavior[A, DeltaA] =
      IBehavior.folded(ev, initial, f)

    override def unionWith[A](a: Event[A])(b: Event[A])(
        f: (A, A) => A): Event[A] =
      Event.fromNode(Event.UnionWith(a, b, f))

    override def collect[B, A](ev: Event[A])(fb: (A) => Option[B]): Event[B] =
      Event.fromNode(Event.Collect(ev, fb))
  }

  private[core] def fromNode[A](n: Push[A]): Event[A] =
    new Event[A] { val node = n }

  def empty[A]: Event[A] = fromNode(new NeverPush[A]())

  def source[A]: EventSource[A] = new EventSource[A](empty[A].node)

  def merge[A](events: Seq[Event[A]]): Event[Seq[A]] = events match {
    case Seq()           => empty
    case Seq(x)          => x.map(Seq(_))
    case Seq(x, xs @ _*) => x.mergeWith(xs: _*)
  }

  // primitive node implementations

  private[core] def snapshotted[A, B](ev: Event[A => B],
                                      snappee: CBehavior[A]): Event[B] =
    fromNode(SnapshotWith(ev, snappee.node))
  private[core] def snapshotted[A, B](ev: Event[A => B],
                                      snappee: DBehavior[A]): Event[B] =
    fromNode(SnapshotWith(ev, snappee.node))
  private[core] def snapshotted[A, B](ev: Event[A => B],
                                      snappee: IBehavior[A, _]): Event[B] =
    fromNode(SnapshotWith(ev, snappee.node))

  // This can't be a case class, we're relying on reference eq to manually place
  // values of different types with different instances of `NeverPush`
  private class NeverPush[A]() extends Push[A] {
    val dependencies                           = List.empty
    def pulse(context: TickContext): Option[A] = None
  }

  private case class SnapshotWith[A, B](
      ev: Event[A => B],
      pullNode: Pull[A]
  ) extends Push[B] {
    val dependencies = List(ev.node, pullNode)
    def pulse(context: TickContext): Option[B] =
      for {
        f     <- context.getPulse(ev.node)
        thunk <- context.getThunk(pullNode)
      } yield f(thunk.force)
  }

  private case class UnionWith[A](
      evA: Event[A],
      evB: Event[A],
      f: (A, A) => A
  ) extends Push[A] {
    val dependencies = List(evA.node, evB.node)

    def pulse(context: TickContext): Option[A] = {
      val aPulse = context.getPulse(evA.node)
      val bPulse = context.getPulse(evB.node)

      (aPulse, bPulse) match {
        case (Some(a), None)    => Some(a)
        case (None, Some(b))    => Some(b)
        case (Some(a), Some(b)) => Some(f(a, b))
        case _                  => None
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
