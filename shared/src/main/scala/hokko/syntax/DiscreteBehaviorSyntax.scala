package hokko.syntax

import hokko.core.DiscreteBehavior
import scala.language.implicitConversions

trait DiscreteBehaviorSyntax extends BehaviorSyntax {
  implicit class DiscreteBehaviorOps[A](
    override val self: DiscreteBehavior[A]
  ) extends BehaviorOps(self) {
    override def map[B](f: A => B): DiscreteBehavior[B] =
      self.reverseApply(DiscreteBehavior.constant(f))
  }
}
