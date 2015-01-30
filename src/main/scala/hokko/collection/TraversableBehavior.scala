package hokko.collection

import TraversableBehavior._
import hokko.core.{ DiscreteBehavior, Event, IncrementalBehavior }
import scala.collection.{ TraversableLike, TraversableOnce }
import scala.collection.generic.CanBuildFrom
import scala.language.{ higherKinds, implicitConversions }

class TraversableBehaviorOps[A, Repr <: TraversableLike[A, Repr], D <: Append[A, Repr]] private[collection] (
  val self: TraversableBehavior[A, Repr, D]
) extends AnyVal {
  def size: DiscreteBehavior[Int] = {
    val initialSize = self.initial.size
    self.deltas.fold(initialSize) { _ + _.sizeDiff }
  }

  def incrementalMap[B, That <: TraversableLike[B, That]](f: A => B)(
    implicit
    cbfMap: CanBuildFrom[Repr, B, That],
    cbfPatch: CanBuildFrom[That, B, That]
  ): IncrementalBehavior[That, Append[B, That]] = {
    val appends = self.deltas.map { _.map(f) }
    val newInitial = self.initial.map(f)
    appends.fold(newInitial) { (acc, diff) => diff.patch(acc) }
  }
}

trait TraversableBehaviorSyntax {
  type TraversableBehavior[A, Repr <: TraversableLike[A, Repr], D <: Append[A, Repr]] = IncrementalBehavior[Repr, D]

  implicit def tbToTbOps[A, Repr <: TraversableLike[A, Repr], D <: Append[A, Repr]](
    self: TraversableBehavior[A, Repr, D]
  ): TraversableBehaviorOps[A, Repr, D] = new TraversableBehaviorOps(self)
}

object TraversableBehavior extends TraversableBehaviorSyntax {
  trait TraversableDiff[A, Repr <: TraversableLike[A, Repr]] extends Diff[A, Repr] {
    def sizeDiff: Int
    def map[B, That <: TraversableLike[B, That]](f: A => B)(
      implicit
      cbf: CanBuildFrom[Repr, A, That]
    ): TraversableDiff[B, That]
  }

  case class Append[A, Repr <: TraversableLike[A, Repr]](additions: Repr) extends Diff[A, Repr] {
    val sizeDiff = additions.size
    def patch(patchee: Repr)(implicit cbf: CanBuildFrom[Repr, A, Repr]): Repr = patchee ++ additions
    def map[B, That <: TraversableLike[B, That]](f: A => B)(
      implicit
      cbf: CanBuildFrom[Repr, B, That]
    ): Append[B, That] = Append(additions.map(f))
  }
}
