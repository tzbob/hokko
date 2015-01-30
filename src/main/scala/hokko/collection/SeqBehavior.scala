package hokko.collection

import SeqBehavior._
import hokko.core.{ DiscreteBehavior, Event, IncrementalBehavior }
import scala.collection.{ TraversableLike, TraversableOnce }
import scala.collection.generic.CanBuildFrom
import scala.language.{ higherKinds, implicitConversions }

class SeqBehaviorOps[A, D[E] <: SeqDiffLike[E, D] with SeqDiff[E]] private[collection] (
  val self: SeqBehavior[A, D]
) extends AnyVal {
  def size: DiscreteBehavior[Int] = {
    val initialSize = self.initial.size
    self.deltas.fold(initialSize) { _ + _.sizeDiff }
  }

  def incrementalMap[B](f: A => B)(
    implicit
    cbfMap: CanBuildFrom[Seq[A], B, Seq[B]],
    cbfId: CanBuildFrom[Seq[B], B, Seq[B]]
  ): SeqBehavior[B, D] = {
    val appends: Event[D[B]] = self.deltas.map { _.map(f) }
    val newInitial = self.initial.map(f)
    appends.fold(newInitial) { (acc, diff) => diff.patch(acc) }
  }

  def +:(heads: Event[A])(implicit cbfId: CanBuildFrom[Seq[A], A, Seq[A]]): SeqBehavior[A, SeqDiff] = {
    val conses: Event[SeqDiff[A]] = heads.map { Cons(_) }
    conses.unionWith(self.deltas)(identity)(x => x: SeqDiff[A]) { (left, right) => Merged(left, right) }
    conses.fold(self.initial) { (acc, diff) => diff.patch(acc) }
  }
}

trait SeqBehaviorSyntax {
  type SeqBehavior[A, D[E] <: SeqDiffLike[E, D]] = IncrementalBehavior[Seq[A], D[A]]
}

object SeqBehavior extends SeqBehaviorSyntax {
  trait SeqDiffLike[A, +This[_]] extends Diff[A, Seq[A]] { self: SeqDiff[A] =>
    def sizeDiff: Int
    def map[B](f: A => B)(implicit cbf: CanBuildFrom[Seq[A], B, Seq[B]]): This[B]
  }

  trait SeqDiff[A] extends SeqDiffLike[A, SeqDiff]

  case class Merged[A](diffs: SeqDiff[A]*) extends SeqDiffLike[A, Merged] with SeqDiff[A] {
    val sizeDiff = diffs.map(_.sizeDiff).sum
    def patch(patchee: Seq[A])(implicit cbf: CanBuildFrom[Seq[A], A, Seq[A]]): Seq[A] =
      diffs.foldLeft(patchee) { (acc, diff) => diff.patch(acc) }
    def map[B](f: A => B)(implicit cbf: CanBuildFrom[Seq[A], B, Seq[B]]): Merged[B] =
      Merged(diffs.map(_.map(f)): _*)
  }

  case class Cons[A](head: A) extends SeqDiffLike[A, Cons] with SeqDiff[A] {
    val sizeDiff = 1
    def patch(patchee: Seq[A])(implicit cbf: CanBuildFrom[Seq[A], A, Seq[A]]): Seq[A] =
      head +: patchee
    def map[B](f: A => B)(implicit cbf: CanBuildFrom[Seq[A], B, Seq[B]]): Cons[B] =
      Cons(f(head))
  }
}