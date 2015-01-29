package hokko.syntax

import hokko.core.DiscreteBehavior
import scala.language.implicitConversions

class DiscreteBehaviorOps[A] private[syntax] (val self: DiscreteBehavior[A]) extends AnyVal {
}

trait DiscreteBehaviorSyntax {
  implicit def ToDiscreteBehaviorOps[A](self: DiscreteBehavior[A]) = new DiscreteBehaviorOps(self)
}
