package hokko.core

import scalaz.{ Applicative, Need, Value }
import hokko.syntax.DiscreteBehaviorSyntax

trait DiscreteBehavior[A] extends Behavior[A] {
  val changes: Event[A]

  def reverseApply[B](fb: DiscreteBehavior[A => B]): DiscreteBehavior[B] =
    DiscreteBehavior.fromNode(DiscreteBehavior.ReverseApply(this, fb))

  def withDeltas[DeltaA](init: A, deltas: Event[DeltaA]): IncrementalBehavior[A, DeltaA] =
    IncrementalBehavior.fromDiscreteAndDeltas(init, this, deltas)
}

object DiscreteBehavior extends DiscreteBehaviorSyntax {
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
    def thunk(context: TickContext): Need[A] = Value(init)
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
      } yield funThunk.value(paramThunk.value)

    def thunk(c: TickContext): Need[B] =
      // we are sure that apParam and apFun already placed their thunks
      Value(c.getPulse(this).orElse(c.getState(this)).get)
  }

  implicit val instance = new Applicative[DiscreteBehavior] {
    def point[A](a: => A): DiscreteBehavior[A] = DiscreteBehavior.constant(a)
    def ap[A, B](fa: => DiscreteBehavior[A])(f: => DiscreteBehavior[A => B]): DiscreteBehavior[B] =
      fa.reverseApply(f)
  }
}
