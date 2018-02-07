package hokko.collection

import hokko.core.{Event, IBehavior}

import scala.collection.generic.CanBuildFrom
import scala.collection.{
  GenTraversable,
  SeqLike,
  TraversableLike,
  immutable => imm
}

object Delta {
  def combineDelta[El, Coll](d1: Delta[El, Coll],
                             d2: Delta[El, Coll]): Delta[El, Coll] =
    (d1, d2) match {
      case (Id(), x) => x
      case (x, Id()) => x
      case _         => Combined(d1, d2)
    }

  def combine[El, Coll](evD1: Event[Delta[El, Coll]],
                        evD2: Event[Delta[El, Coll]]): Event[Delta[El, Coll]] =
    evD1.unionWith(evD2)(Delta.combineDelta)

  def foldApply[A0, Repr](
      traversable: Repr,
      newDeltas: Event[Delta[A0, Repr]]): IBehavior[Repr, Delta[A0, Repr]] =
    newDeltas.fold(traversable) { (acc, delta) =>
      delta(acc)
    }

  def applyFoldUndo[Acc, Repr, A0](acc: Acc,
                                   d: Delta[A0, Repr],
                                   op: (Acc, A0) => Acc,
                                   undo: (Acc, A0) => Acc): Acc = {
    d match {
      case Id() => acc
      case Combined(d1, d2) =>
        applyFoldUndo(applyFoldUndo(acc, d1, op, undo), d2, op, undo)
      case Concat(others)                 => others.foldLeft(acc)(op)
      case u @ Updated(_, previous, next) => undo(op(acc, next), previous)
      case p @ Prepend(el)                => op(acc, el)
      case a @ Append(el)                 => op(acc, el)
    }
  }

  def map[A, B, Repr[_]](delta: Delta[A, Repr[A]], f: A => B)(
      implicit cbfMap: CanBuildFrom[Repr[A], B, Repr[B]],
      cbfDelta: CanBuildFrom[Repr[B], B, Repr[B]]): Delta[B, Repr[B]] =
    delta match {
      case Id()                 => Id()
      case Combined(d1, d2)     => combineDelta(map(d1, f), map(d2, f))
      case c @ Concat(_)        => c.map(f)
      case u @ Updated(_, _, _) => u.map(f)
      case p @ Prepend(_)       => p.map(f)
      case a @ Append(_)        => a.map(f)
    }
}

sealed trait Delta[+El, Coll] {
  def apply(c: Coll): Coll
  def mapIndex(f: Int => Int): Delta[El, Coll]
}

case class Id[Coll]() extends Delta[Nothing, Coll] {
  def apply(c: Coll): Coll = c

  def mapIndex(f: (Int) => Int) = this
}

case class Combined[El, Coll] private (d1: Delta[El, Coll],
                                       d2: Delta[El, Coll])
    extends Delta[El, Coll] {
  override def apply(c: Coll): Coll = d2(d1(c))
  override def mapIndex(f: (Int) => Int): Delta[El, Coll] =
    Delta.combineDelta(d1.mapIndex(f), d2.mapIndex(f))
}

case class Concat[A0, Repr[A] <: TraversableLike[A, Repr[A]]](
    that: GenTraversable[A0])(
    implicit canBuildFrom: CanBuildFrom[Repr[A0], A0, Repr[A0]]
) extends Delta[A0, Repr[A0]] {
  override def apply(c: Repr[A0]): Repr[A0] = c ++ that
  override def mapIndex(f: (Int) => Int)    = this

  def map[B](f: A0 => B)(
      implicit cbfMap: CanBuildFrom[Repr[B], B, Repr[B]]): Concat[B, Repr] =
    Concat(that.map(f))
}

case class Append[A0, Repr[A] <: SeqLike[A, Repr[A]]](el: A0)(
    implicit canBuildFrom: CanBuildFrom[Repr[A0], A0, Repr[A0]]
) extends Delta[A0, Repr[A0]] {
  def apply(c: Repr[A0]): Repr[A0]                   = c :+ el
  def mapIndex(f: (Int) => Int): Delta[A0, Repr[A0]] = ???

  def map[B](f: A0 => B)(
      implicit cbfMap: CanBuildFrom[Repr[B], B, Repr[B]]): Append[B, Repr] =
    Append(f(el))
}

case class Prepend[A0, Repr[A] <: SeqLike[A, Repr[A]]](el: A0)(
    implicit canBuildFrom: CanBuildFrom[Repr[A0], A0, Repr[A0]]
) extends Delta[A0, Repr[A0]] {
  def apply(c: Repr[A0]): Repr[A0]                   = el +: c
  def mapIndex(f: (Int) => Int): Delta[A0, Repr[A0]] = ???

  def map[B](f: A0 => B)(
      implicit cbfMap: CanBuildFrom[Repr[B], B, Repr[B]]): Prepend[B, Repr] =
    Prepend(f(el))
}

case class Updated[A0, Repr[A] <: SeqLike[A, Repr[A]]](index: Int,
                                                       previous: A0,
                                                       next: A0)(
    implicit cbf: CanBuildFrom[Repr[A0], A0, Repr[A0]]
) extends Delta[A0, Repr[A0]] {
  override def apply(c: Repr[A0]): Repr[A0] =
    c.updated(index, next)

  override def mapIndex(f: (Int) => Int) = this.copy(index = f(this.index))

  def map[B](f: A0 => B)(
      implicit cbfMap: CanBuildFrom[Repr[B], B, Repr[B]]): Updated[B, Repr] = {
    Updated(index, f(previous), f(next))
  }
}
