package hokko.core

import scalaz.Need
import scalaz.std.option._
import scalaz.syntax.applicative._
import scalaz.Functor

trait Event[A] {
  private[hokko] val node: Push[A]

  // core

  def fold[B](initial: B)(f: (B, A) => B): DiscreteBehavior[B] =
    DiscreteBehavior.folded(this, initial, f)

  def unionWithBehavior[B, C](b: Event[B], f1: Behavior[A => C], f2: Behavior[B => C], f3: Behavior[(A, B) => C]): Event[C] =
    Event.fromNode(Event.UnionWith(this, b, f1, f2, f3))

  def collectWithBehavior[B](fb: Behavior[A => Option[B]]): Event[B] =
    Event.fromNode(Event.CollectWithBehavior(this, fb))

  // derived functions

  def map[B](f: A => B): Event[B] =
    collectWithBehavior(Behavior.constant { (a: A) =>
      Some(f(a))
    })

  def dropIf[B](f: A => Boolean): Event[A] =
    collectWithBehavior(Behavior.constant { (a: A) =>
      if (f(a)) None else Some(a)
    })
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
    fb1: Behavior[A => C],
    fb2: Behavior[B => C],
    fb3: Behavior[(A, B) => C]
  ) extends Push[C] {
    val dependencies = List(evA.node, evB.node, fb1.node, fb2.node, fb3.node)

    def pulse(context: TickContext): Option[C] = {
      val aPulse = context.getPulse(evA.node)
      val bPulse = context.getPulse(evB.node)

      // we can be sure that the thunks were already filled in
      def now[T](b: Behavior[T]): Need[T] = context.getThunk(b.node).get

      val f1 = now(fb1)
      val f2 = now(fb2)
      val f3 = now(fb3)

      (aPulse, bPulse) match {
        case (Some(a), None) => Some(f1.value(a))
        case (None, Some(b)) => Some(f2.value(b))
        case (Some(a), Some(b)) => Some(f3.value(a, b))
        case _ => None
      }
    }

  }

  private case class CollectWithBehavior[A, B](
    ev: Event[A],
    b: Behavior[A => Option[B]]
  ) extends Push[B] {
    val dependencies = List(ev.node, b.node)
    def pulse(context: TickContext): Option[B] =
      for {
        pulse <- context.getPulse(ev.node)
        thunk <- context.getThunk(b.node)
        newPulse <- thunk.value(pulse)
      } yield newPulse
  }

  implicit val evtFunctor = new Functor[Event] {
    def map[A, B](fa: Event[A])(f: A => B): Event[B] =
      fa.map(f)
  }
}
