package hokko.core

import scalaz.{Applicative, Need}
import scalaz.std.option._
import scalaz.syntax.applicative._

trait Behavior[A] {
  private[core] val node: Pull[A]

  // core

  def reverseApply[B](fb: Behavior[A => B]): Behavior[B] =
    Behavior.ReverseApply(this, fb)

  def snapshotWith[B](ev: Event[A => B]): Event[B] =
    Event.snapshotted(ev, this)

  // derived 

}

object Behavior {
  def constant[A](x: A): Behavior[A] = DiscreteBehavior.constant(x)

  private case class Polling[A](f: () => A) extends Behavior[A] {
    val node = new Pull[A] {
      val dependencies = List.empty[Node[_]]
      def thunk(context: TickContext) = Need(f())
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
        } yield bThunk <*> fbThunk
        // if dependencies are listed we are sure they are available
        opt.get
      }
    }
  }

  implicit val instance = new Applicative[Behavior] {
    def point[A](a: => A): Behavior[A] = Behavior.constant(a)
    def ap[A, B](fa: => Behavior[A])(f: => Behavior[A => B]): Behavior[B] =
      fa.reverseApply(f)
  }
}
