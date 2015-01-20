package hokko.core

import scala.language.implicitConversions
import scalaz.{ Applicative, Need, Value }

trait DiscreteBehavior[A] extends Behavior[A] {
  private[core] val node: Push[A] with Pull[A]

  // primitives 

  // lazy so that implementations have a chance to implement node
  lazy val changes: Event[A] = Event.fromNode(node)

  def reverseApply[B](fb: DiscreteBehavior[A => B]): DiscreteBehavior[B] =
    DiscreteBehavior.fromNode(DiscreteBehavior.ReverseApply(this, fb))

  // derived methods
}

object DiscreteBehavior {
  // Convenience traits stacked in the right order
  private trait PushState[A] extends Push[A] with State[A] {
    def state(context: TickContext): Option[A] =
      context.getPulse(this)
  }
  private trait PullStatePush[A] extends PushState[A] with Pull[A]

  private def fromNode[A](n: Push[A] with Pull[A]): DiscreteBehavior[A] =
    new DiscreteBehavior[A] { val node = n }

  def constant[A](init: A): DiscreteBehavior[A] = fromNode(ConstantNode(init))

  // primitive node implementations
  private[core] def folded[A, B](foldee: Event[A], initial: B, f: (B, A) => B): DiscreteBehavior[B] =
    fromNode(FoldNode(foldee, initial, f))

  private case class ConstantNode[A](init: A) extends Push[A] with Pull[A] {
    val dependencies = List.empty
    def pulse(context: TickContext): Option[A] = None
    def thunk(context: TickContext): Need[A] = Value(init)
  }

  private case class FoldNode[A, B](
    ev: Event[A],
    init: B,
    f: (B, A) => B
  ) extends PullStatePush[B] {
    val dependencies = List(ev.node)
    def pulse(context: TickContext): Option[B] = {
      val evPulse = context.getPulse(ev.node)
      evPulse.map { pulse =>
        val previous = context.getState(this).getOrElse(init)
        f(previous, pulse)
      }
    }
    def thunk(c: TickContext): Need[B] =
      Value(c.getPulse(this).orElse(c.getState(this)).getOrElse(init))
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
