package hokko.syntax

import hokko.core.Event
import scala.language.implicitConversions

class EventOps[A] private[syntax] (val self: Event[A]) extends AnyVal {
  def map[B](f: A => B): Event[B] =
    self.collect { a => Some(f(a)) }

  def dropIf[B](f: A => Boolean): Event[A] =
    self.collect { a => if (f(a)) None else Some(a) }
}

trait EventSyntax {
  implicit def ToEventOps[A](self: Event[A]) = new EventOps(self)
}
