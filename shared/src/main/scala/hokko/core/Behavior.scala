package hokko.core

import hokko.syntax.BehaviorSyntax
import scala.scalajs.js.annotation.JSExport
import scalajs2jsscala.annotation.JsScalaProxy

@JsScalaProxy
trait Behavior[+A] {
  private[core] val node: Pull[A]

  @JSExport
  def reverseApply[B, AA >: A](fb: Behavior[AA => B]): Behavior[B] =
    Behavior.ReverseApply(this, fb)

  @JSExport
  def snapshotWith[B, AA >: A](ev: Event[AA => B]): Event[B] =
    Event.snapshotted(ev, this)

  @JSExport
  def withChanges[AA >: A](changes: Event[AA]): DiscreteBehavior[AA] =
    DiscreteBehavior.fromBehaviorAndChanges(this, changes)
}

object Behavior extends BehaviorSyntax {
  /*
   fromPollWithDeps[A](list: Node[A], f: TickContext => IO[A]): Behavior[A]
   Event[Future[A]].wormhole: Event[A]
   */

  @JSExport
  def constant[A](x: A): Behavior[A] = DiscreteBehavior.constant(x)

  @JSExport
  def fromPoll[A](f: () => A): Behavior[A] = new Behavior[A] {
    val node = new Pull[A] {
      val dependencies = List.empty[Node[_]]
      def thunk(context: TickContext) = Thunk(f())
    }
  }

  private case class ReverseApply[A, B](
    b: Behavior[A],
    fb: Behavior[A => B]
  ) extends Behavior[B] {
    val node = new Pull[B] {
      val dependencies = List(b.node, fb.node)
      def thunk(context: TickContext) = {
        val opt = for {
          bThunk <- context.getThunk(b.node)
          fbThunk <- context.getThunk(fb.node)
        } yield for { param <- bThunk; fun <- fbThunk } yield fun(param)
        // if dependencies are listed we are sure they are available
        opt.get
      }
    }
  }
}