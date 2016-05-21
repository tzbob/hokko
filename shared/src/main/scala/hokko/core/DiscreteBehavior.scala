package hokko.core

import cats.Applicative

trait DiscreteBehavior[+A] extends Behavior[A] {
  def changes(): Event[A]

  def discreteReverseApply[B, AA >: A](fb: DiscreteBehavior[A => B]): DiscreteBehavior[B] =
    DiscreteBehavior.fromNode(DiscreteBehavior.ReverseApply(this, fb))

  def withDeltas[DeltaA, AA >: A](init: AA, deltas: Event[DeltaA]): IncrementalBehavior[AA, DeltaA] =
    IncrementalBehavior.fromDiscreteAndDeltas(init, this, deltas)
}

object DiscreteBehavior {
  implicit val applicativeInstance: Applicative[DiscreteBehavior] =
    new Applicative[DiscreteBehavior] {
      def pure[A](x: A): DiscreteBehavior[A] = constant(x)
      def ap[A, B](ff: DiscreteBehavior[A => B])(fa: DiscreteBehavior[A]): DiscreteBehavior[B] =
        fa.discreteReverseApply(ff)
    }

  def constant[A](init: A): DiscreteBehavior[A] = fromNode(ConstantNode(init))

  // Convenience traits stacked in the right order
  private[core] trait PushState[A] extends Push[A] with State[A] {
    def state(context: TickContext): Option[A] =
      context.getPulse(this)
  }
  private[core] trait PullStatePush[A] extends PushState[A] with Pull[A]

  private[core] def fromBehaviorAndChanges[A](b: Behavior[A], ev: Event[A]) =
    new DiscreteBehavior[A] {
      val node = b.node
      val changes: Event[A] = ev
    }

  private def fromNode[A](n: Push[A] with Pull[A]) =
    new DiscreteBehavior[A] {
      val node = n
      val changes: Event[A] = Event.fromNode(n)
    }

  // primitive node implementations
  private case class ConstantNode[A](init: A) extends Push[A] with Pull[A] {
    val dependencies = List.empty
    def pulse(context: TickContext): Option[A] = None
    def thunk(context: TickContext): Thunk[A] = Thunk.eager(init)
  }

  private case class ReverseApply[A, B](
    apParam: DiscreteBehavior[A],
    apFun: DiscreteBehavior[A => B]
  ) extends PullStatePush[B] {
    val dependencies = List(apParam.node, apFun.node)
    def pulse(context: TickContext): Option[B] =
      for {
        paramThunk <- context.getThunk(apParam.node)
        funThunk <- context.getThunk(apFun.node)
      } yield funThunk.force(paramThunk.force)

    def thunk(c: TickContext): Thunk[B] =
      // we are sure that apParam and apFun already placed their thunks
      Thunk.eager(c.getPulse(this).orElse(c.getState(this)).get)
  }

}
