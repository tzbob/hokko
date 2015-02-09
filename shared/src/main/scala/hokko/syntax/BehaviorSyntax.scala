package hokko.syntax

import hokko.core.{ Behavior, DiscreteBehavior, Event }
import scala.language.implicitConversions

trait BehaviorSyntax {
  implicit class BehaviorOps[A](val self: Behavior[A]) {
    def signalChanges(signals: Event[Unit]): DiscreteBehavior[A] = {
      val ev: Event[A => A] = signals.map { _ => identity }
      val snapped = self.snapshotWith(ev)
      self.withChanges(snapped)
    }
    def map[B](f: A => B): Behavior[B] = self.reverseApply(Behavior.constant(f))
  }
}
