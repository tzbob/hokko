package hokko.core.tc

import cats.Functor

trait Event[Ev[_], IBeh[_, _]] extends Functor[Ev] {

  def fold[A, DeltaA](ev: Ev[DeltaA], initial: A)(
      f: (A, DeltaA) => A): IBeh[A, DeltaA]

  def unionWith[B, C, A](a: Ev[A])(b: Ev[B])(f1: A => C)(f2: B => C)(
      f3: (A, B) => C): Ev[C]

  def collect[B, A](ev: Ev[A])(fb: A => Option[B]): Ev[B]

  // Derived Ops

  def hold[A](ev: Ev[A], initial: A): IBeh[A, A] =
    fold(ev, initial) { (_, n) =>
      n
    }

  def unionLeft[A](first: Ev[A], other: Ev[A]): Ev[A] =
    unionWith(first)(other)(identity)(identity) { (left, _) =>
      left
    }

  def unionRight[A](other: Ev[A], first: Ev[A]): Ev[A] =
    unionLeft(first, other)

  def mergeWith[A](ev: Ev[A], events: Ev[A]*): Ev[Seq[A]] = {
    val selfSeq: Ev[Seq[A]] = this.map(ev)(Seq(_))
    events.foldLeft(selfSeq) { (acc, event) =>
      unionWith(acc)(event)(identity)(Seq(_))(_ :+ _)
    }
  }

  def map[A, B](ev: Ev[A])(f: A => B): Ev[B] =
    collect(ev) { a =>
      Some(f(a))
    }

  def dropIf[A](ev: Ev[A])(f: A => Boolean): Ev[A] =
    collect(ev) { a =>
      if (f(a)) None else Some(a)
    }
}
