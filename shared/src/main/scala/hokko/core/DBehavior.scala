package hokko.core

import cats.Applicative
import cats.syntax.{ApplicativeSyntax, ApplySyntax}
import hokko.syntax.SnapshottableSyntax

trait DBehavior[+A] {
  private[core] val node: Pull[A]

  def changes(): Event[A]

  def withDeltas[DeltaA, AA >: A](
      init: AA,
      deltas: Event[DeltaA]): IBehavior[AA, DeltaA] =
    IBehavior.fromDiscreteAndDeltas(init, this, deltas)

  def toCBehavior: CBehavior[A] = new CBehavior[A] {
    override private[core] val node: Pull[A] = DBehavior.this.node
  }
}

object DBehavior
    extends ApplicativeSyntax
    with ApplySyntax
    with SnapshottableSyntax {
  implicit val hokkoDBehaviorInstances: tc.Snapshottable[DBehavior, Event] with Applicative[
    DBehavior] =
    new tc.Snapshottable[DBehavior, Event] with Applicative[DBehavior] {
      def pure[A](x: A): DBehavior[A] = constant(x)
      def ap[A, B](ff: DBehavior[A => B])(fa: DBehavior[A]): DBehavior[B] =
        DBehavior.this.ap(ff)(fa)
      def snapshotWith[A, B, C](b: DBehavior[A], ev: Event[B])(
          f: (A, B) => C): Event[C] =
        Event.snapshotted(ev.map(x => (bv: A) => f(bv, x)), b)
    }

  def constant[A](init: A): DBehavior[A] =
    fromNode(ConstantNode(init))

  def ap[A, B](ff: DBehavior[A => B])(fa: DBehavior[A]): DBehavior[B] =
    fromNode(ReverseApply(fa, ff))

  // Convenience traits stacked in the right order
  private[core] trait PushState[A] extends Push[A] with State[A] {
    def state(context: TickContext): Option[A] =
      context.getPulse(this)
  }
  private[core] trait PullStatePush[A] extends PushState[A] with Pull[A]

  private[core] def fromBehaviorAndChanges[A](b: CBehavior[A], ev: Event[A]) =
    new DBehavior[A] {
      val node              = b.node
      val changes: Event[A] = ev
    }

  private def fromNode[A](n: Push[A] with Pull[A]) =
    new DBehavior[A] {
      val node              = n
      val changes: Event[A] = Event.fromNode(n)
    }

  // primitive node implementations
  private case class ConstantNode[A](init: A) extends Push[A] with Pull[A] {
    val dependencies                           = List.empty
    def pulse(context: TickContext): Option[A] = None
    def thunk(context: TickContext): Thunk[A]  = Thunk.eager(init)
  }

  private case class ReverseApply[A, B](
      apParam: DBehavior[A],
      apFun: DBehavior[A => B]
  ) extends PullStatePush[B] {
    val dependencies = List(apParam.node, apFun.node)
    def pulse(context: TickContext): Option[B] =
      for {
        paramThunk <- context.getThunk(apParam.node)
        funThunk   <- context.getThunk(apFun.node)
      } yield funThunk.force(paramThunk.force)

    def thunk(c: TickContext): Thunk[B] =
      // we are sure that apParam and apFun already placed their thunks
      Thunk.eager(c.getPulse(this).orElse(c.getState(this)).get)
  }

}
