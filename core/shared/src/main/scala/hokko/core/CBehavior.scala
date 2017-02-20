package hokko.core

import cats.syntax.{ApplicativeSyntax, ApplySyntax, FunctorSyntax}
import cats.{Applicative, Apply, Functor}
import hokko.syntax.SnapshottableSyntax

trait CBehavior[+A] extends Primitive[A] {
  override private[core] val node: Pull[A]
}

class CBehaviorSource[A](default: A) extends CBehavior[A] {
  private var source = Option.empty[() => Option[A]]

  def changeSource(source: => Option[A]): Unit =
    this.source = Some(() => source)

  def unSet(): Unit = this.source = None

  override private[core] val node: Pull[A] = CBehavior.fromPoll { () =>
    source
      .map { f =>
        f().getOrElse(default)
      }
      .getOrElse(default)
  }.node
}

object CBehaviorSource {
  implicit def hokkoCBSrcToFunctorOps[A](target: CBehaviorSource[A])(
      implicit tc: cats.Functor[CBehavior]): Functor.Ops[CBehavior, A] =
    CBehavior.toFunctorOps(target)

  implicit def hokkoCBSrcSyntaxApply[A](fa: CBehaviorSource[A])(
      implicit F: Apply[CBehavior]): Apply.Ops[CBehavior, A] =
    CBehavior.catsSyntaxApply(fa)
}

object CBehavior
    extends ApplicativeSyntax
    with ApplySyntax
    with FunctorSyntax
    with SnapshottableSyntax[Event, CBehavior] {

  implicit val hokkoCBehaviorInstances: tc.Snapshottable[CBehavior, Event] with Applicative[
    CBehavior] =
    new tc.Snapshottable[CBehavior, Event] with Applicative[CBehavior] {
      def pure[A](x: A): CBehavior[A] = constant(x)

      def ap[A, B](ff: CBehavior[A => B])(fa: CBehavior[A]): CBehavior[B] =
        CBehavior.this.ap(ff)(fa)

      def snapshotWith[A, B, C](b: CBehavior[A], ev: Event[B])(
          f: (A, B) => C): Event[C] =
        Event.snapshotted(ev.map(x => (bv: A) => f(bv, x)), b)
    }

  def constant[A](x: A): CBehavior[A] = new CBehavior[A] {
    override private[core] val node: Pull[A] = new Pull[A] {
      override def thunk(context: TickContext): Thunk[A] = Thunk.eager(x)
      override val dependencies: List[Node[_]]           = List.empty
    }
  }

  def ap[A, B](ff: CBehavior[A => B])(fa: CBehavior[A]): CBehavior[B] =
    new CBehavior[B] {
      override private[core] val node: Pull[B] = new Pull[B] {
        override def thunk(context: TickContext): Thunk[B] = {
          val opt = for {
            bThunk  <- context.getThunk(fa.node)
            fbThunk <- context.getThunk(ff.node)
          } yield for { param <- bThunk; fun <- fbThunk } yield fun(param)
          // if dependencies are listed we are sure they are available
          opt.get
        }

        override val dependencies: List[Node[_]] = List(fa.node, ff.node)
      }
    }

  def fromPoll[A](f: () => A): CBehavior[A] = new CBehavior[A] {
    val node = new Pull[A] {
      val dependencies                = List.empty[Node[_]]
      def thunk(context: TickContext) = Thunk(f())
    }
  }

  def source[A](default: A): CBehaviorSource[A] =
    new CBehaviorSource[A](default)

}
