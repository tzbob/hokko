package hokko.syntax

import hokko.core.{ Behavior, DiscreteBehavior, Event }
import scala.language.implicitConversions

class BehaviorOps[A] private[syntax] (val self: Behavior[A]) extends AnyVal {
  def signalChanges(signals: Event[Unit]): DiscreteBehavior[A] = {
    val ev: Event[A => A] = signals.map { _ => identity }
    val snapped = self.snapshotWith(ev)
    self.withChanges(snapped)
  }
}

trait BehaviorSyntax {
  implicit def ToBehaviorOps[A](self: Behavior[A]) = new BehaviorOps(self)
}
