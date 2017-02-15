package hokko.syntax

import hokko.core.tc.Event

trait EventSyntax[Ev[_], IBeh[_, _]] {
  implicit def syntaxEvent[A](e: Ev[A])(
      implicit ev: Event[Ev, IBeh]): EventOps[Ev, IBeh, A] =
    new EventOps[Ev, IBeh, A](e)
}

final class EventOps[Ev[_], IBeh[_, _], A](e: Ev[A])(
    implicit ev: Event[Ev, IBeh]) {

  def fold[AccA](initial: AccA)(f: (AccA, A) => AccA): IBeh[AccA, A] =
    ev.fold(e, initial)(f)

  def unionWith(b: Ev[A])(f: (A, A) => A): Ev[A] =
    ev.unionWith(e)(b)(f)

  def collect[B](fb: A => Option[B]): Ev[B] =
    ev.collect(e)(fb)

  // Derived Ops

  def hold(initial: A): IBeh[A, A] =
    ev.hold(e, initial)

  def unionLeft(other: Ev[A]): Ev[A] =
    ev.unionLeft(e, other)

  def unionRight(first: Ev[A]): Ev[A] =
    ev.unionRight(e, first)

  def mergeWith(events: Ev[A]*): Ev[Seq[A]] =
    ev.mergeWith(e, events: _*)

  def dropIf(f: A => Boolean): Ev[A] =
    ev.dropIf(e)(f)
}
