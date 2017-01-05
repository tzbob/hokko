package hokko.collection

import hokko.core.{Event, IBehavior}

import scala.collection.generic.{CanBuildFrom, IsSeqLike, IsTraversableLike}
import scala.collection.{
  GenSeq,
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
      case Concat(others) => others.foldLeft(acc)(op)
      case p @ Patch(_, patch, _, removed) =>
        val accWithNewElements = patch.foldLeft(acc)(op)
        removed.foldLeft(accWithNewElements)(undo)
    }
  }

  def map[A, B, Repr[_]](delta: Delta[A, Repr[A]], f: A => B)(
      implicit cbfMap: CanBuildFrom[Repr[A], B, Repr[B]],
      cbfDelta: CanBuildFrom[Repr[B], B, Repr[B]]): Delta[B, Repr[B]] =
    delta match {
      case Id()                  => Id()
      case Combined(d1, d2)      => combineDelta(map(d1, f), map(d2, f))
      case c @ Concat(_)         => c.map(f)
      case p @ Patch(_, _, _, _) => p.map(f)
    }

//  def filter[A, Repr[_]](delta: Delta[A, Repr[A]],
//                         p: A => Boolean): Delta[A, Repr[A]] =
//    delta match {
//      case Id()                  => Id()
//      case Combined(d1, d2)      => combineDelta(filter(d1, p), filter(d2, p))
//      case c @ Concat(_)         => c.filter(p)
//      case p @ Patch(_, _, _, _) => p.filter(p)
//    }
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

//  def filter[B](p: A0 => B)(implicit cbfMap: CanBuildFrom[Repr[B], B, Repr[B]])
//    : Delta[B, Repr[B]] = {
//    val filtered = that.filter(p)
//    if (filtered.isEmpty) Id()
//    else Concat(filtered)
//  }

}

case class Patch[A0, Repr[A] <: SeqLike[A, Repr[A]]](
    from: Int,
    patch: GenSeq[A0],
    replaced: Int,
    removedElements: TraversableOnce[A0])(
    implicit cbf: CanBuildFrom[Repr[A0], A0, Repr[A0]]
) extends Delta[A0, Repr[A0]] {
  override def apply(c: Repr[A0]): Repr[A0] =
    c.patch(from, patch, replaced)

  override def mapIndex(f: (Int) => Int) = this.copy(from = f(this.from))

  def map[B](f: A0 => B)(
      implicit cbfMap: CanBuildFrom[Repr[B], B, Repr[B]]): Patch[B, Repr] = {
    Patch(from, patch.map(f), replaced, removedElements.map(f))
  }

//  def filter[B](p: A0 => B)(implicit cbfMap: CanBuildFrom[Repr[B], B, Repr[B]])
//    : Delta[B, Repr[B]] = {
//    val filtered = patch.filter(p)
//    if (filtered.isEmpty) Id()
//    else {}
//  }
}

//
////  def filter[A](f: A => Boolean)(d: Delta[A]): Delta[A] = d match {
////    case Empty => Empty
////    case Update(pel, nel, idx) =>
////      if (f(nel)) Update(pel, nel, idx)
////      else Remove(pel, idx)
////    case Insert(el, idx) =>
////      if (f(el)) Insert(el, idx)
////      else Remove(el, idx)
////    case Remove(el, idx) =>
////      Remove(el, idx)
////    case Combined(d1, d2) => combine(filter(f)(d), filter(f)(d))
////  }
//
//  def map[A, B](d: Delta[A])(f: (A, Int) => (B, Int)): Delta[B] =
//    d match {
//      case Combined(d1, d2) => combine(map(d1)(f), map(d2)(f))
//      case Update(pel, nel, idx) =>
//        val (pel0, newIdx) = f(pel, idx)
//        val (nel0, _)      = f(nel, idx)
//        Update(pel0, nel0, newIdx)
//      case Insert(a, idx) =>
//        val (b, newIdx) = f(a, idx)
//        Insert(b, newIdx)
//      case Remove(a, idx) =>
//        val (b, newIdx) = f(a, idx)
//        Remove(b, newIdx)
//      case Empty => Empty
//    }
//
//  def mapIndex[A](d: Delta[A])(f: Int => Int): Delta[A] =
//    map(d) { (el, idx) =>
//      (el, f(idx))
//    }
//
//  def mapElement[A, B](d: Delta[A])(f: A => B): Delta[B] =
//    map(d) { (el, idx) =>
//      (f(el), idx)
//    }
//
//  def translate[A](d: Delta[A], sum: Int): Delta[A] =
//    mapIndex(d)(_ + sum)
