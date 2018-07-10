package hokko.core

import cats.Applicative
import cats.syntax.{ApplicativeSyntax, ApplySyntax, FunctorSyntax}
import hokko.syntax.SnapshottableSyntax

trait DBehavior[A] extends Primitive[A] {
  override private[core] val node: Pull[A]
  val init: A

  def changes(): Event[A]

  def toCBehavior[AA >: A]: CBehavior[A] = new CBehavior[A] {
    override private[core] val node: Pull[A] = DBehavior.this.node
  }

  def toIBehavior[DeltaA](diff: (A, A) => DeltaA)(
      patch: (A, DeltaA) => A): IBehavior[A, DeltaA] = {
    val memorizedDeltas = changes.fold(Option.empty[DeltaA] -> init) {
      case ((_, oldValue), newValue) =>
        Some(diff(newValue, oldValue)) -> newValue
    }

    val diffs = memorizedDeltas.changes.collect(_._1)
    diffs.fold(init)(patch)
  }

}

object DBehavior
    extends ApplicativeSyntax
    with ApplySyntax
    with FunctorSyntax
    with SnapshottableSyntax[Event, DBehavior] {
  implicit val hokkoDBehaviorInstances
    : tc.Snapshottable[DBehavior, Event] with Applicative[DBehavior] =
    new tc.Snapshottable[DBehavior, Event] with Applicative[DBehavior] {
      def pure[A](x: A): DBehavior[A] = constant(x)
      def ap[A, B](ff: DBehavior[A => B])(fa: DBehavior[A]): DBehavior[B] =
        DBehavior.this.ap(ff)(fa)
      def snapshotWith[A, B, C](b: DBehavior[A], ev: Event[B])(
          f: (A, B) => C): Event[C] =
        Event.snapshotted(ev.map(x => (bv: A) => f(bv, x)), b)
    }

  def constant[A](init: A): DBehavior[A] =
    fromNode(init, ConstantNode(init))

  def ap[A, B](ff: DBehavior[A => B])(fa: DBehavior[A]): DBehavior[B] =
    fromNode(ff.init(fa.init), ReverseApply(fa, ff))

  def delayed[A](target: => DBehavior[A], initial: A): DBehavior[A] = {

    val statePush = new PushState[A] {
      // level is normal, updates happen after 'pull'
      lazy val dependencies = List(target.changes.node)

      def pulse(context: TickContext): Option[A] =
        context.getPulse(target.changes.node)
    }

    val pull = new Pull[A] {
      // level is early so early state is pulled
      override val delayed  = true
      lazy val dependencies = List(target.node, statePush)

      // retrieve previous state
      def thunk(context: TickContext): Thunk[A] =
        Thunk.eager(context.getState(statePush).getOrElse(initial))
    }

    fromNode(initial, statePush, pull)
  }

  // Convenience traits stacked in the right order
  private[core] trait PushState[A] extends Push[A] with State[A] {
    def state(context: TickContext): Option[A] =
      context.getPulse(this)
  }
  private[core] trait PullStatePush[A] extends PushState[A] with Pull[A]

  private def fromNode[A](initial: A, push: Push[A], pull: Pull[A]) =
    new DBehavior[A] {
      val init              = initial
      val node              = pull
      val changes: Event[A] = Event.fromNode(push)
    }

  private def fromNode[A](initial: A, n: Push[A] with Pull[A]) =
    new DBehavior[A] {
      val init              = initial
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
      Thunk.eager(c.getState(this).get)
  }

}
