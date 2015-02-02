package hokko.collection

import hokko.core.{ Behavior, DiscreteBehavior, Event, IncrementalBehavior }
import scala.collection.{ TraversableLike, TraversableOnce }
import scala.collection.generic.CanBuildFrom
import scala.language.{ higherKinds, implicitConversions }

class SetBehaviorOps[A, D[E] <: SetBehavior.SetDiffLike[E, D] with SetBehavior.SetDiff[E]] private[collection] (
  val self: IncrementalBehavior[Set[A], D[A]]
) extends AnyVal {
  import SetBehavior._

  def incrementalMap[B](f: A => B)(
    implicit
    cbfMap: CanBuildFrom[Set[A], B, Set[B]],
    cbfId: CanBuildFrom[Set[B], B, Set[B]]
  ): SetBehavior[B, D] = {
    val appends: Event[D[B]] = self.deltas.map { _.map(f) }
    val newInitial = self.initial.map(f)
    appends.fold(newInitial) { (acc, diff) => diff.patch(acc) }
  }

  private def selfPatch[B](evt: Event[B])(f: B => SetDiff[A]): SetBehavior[A, SetDiff] = {
    val conses = evt.map(f)
    val merged = conses.unionWith(self.deltas)(identity)(x => x: SetDiff[A]) { (left, right) => Merged(left, right) }
    merged.fold(self.initial) { (acc, diff) => diff.patch(acc) }
  }

  def +(additions: Event[A])(implicit cbfId: CanBuildFrom[Set[A], A, Set[A]]): SetBehavior[A, SetDiff] =
    selfPatch(additions)(Add(_))

  def -(removals: Event[A])(implicit cbfId: CanBuildFrom[Set[A], A, Set[A]]): SetBehavior[A, SetDiff] =
    selfPatch(removals)(Remove(_))
}

object SetBehavior {
  def empty[A]: SetBehavior[A, Nothing] =
    fromSet(Set.empty[A])
  def fromSet[A, D[E] <: SetDiffLike[E, D] with SetDiff[E]](set: Set[A]): SetBehavior[A, D] =
    IncrementalBehavior.constant(set)

  trait SetBehaviorSyntax {
    type SetBehavior[A, D[E] <: SetDiffLike[E, D] with SetDiff[E]] = IncrementalBehavior[Set[A], D[A]]
    implicit def ToSetBehaviorOps[A, D[E] <: SetDiffLike[E, D] with SetDiff[E]](self: SetBehavior[A, D]) =
      new SetBehaviorOps(self)
  }

  sealed trait SetDiffLike[A, +This[_]] extends Diff[A, Set[A]] {
    def map[B](f: A => B)(implicit cbf: CanBuildFrom[Set[A], B, Set[B]]): This[B]
  }

  sealed trait SetDiff[A] extends SetDiffLike[A, SetDiff]

  case class Merged[A](diffs: SetDiff[A]*) extends SetDiffLike[A, Merged] with SetDiff[A] {
    def patch(patchee: Set[A])(implicit cbf: CanBuildFrom[Set[A], A, Set[A]]): Set[A] =
      diffs.foldLeft(patchee) { (acc, diff) => diff.patch(acc) }
    def map[B](f: A => B)(implicit cbf: CanBuildFrom[Set[A], B, Set[B]]): Merged[B] =
      Merged(diffs.map(_.map(f)): _*)
  }

  case class Add[A](addition: A) extends SetDiffLike[A, Add] with SetDiff[A] {
    def patch(patchee: Set[A])(implicit cbf: CanBuildFrom[Set[A], A, Set[A]]): Set[A] =
      patchee + addition
    def map[B](f: A => B)(implicit cbf: CanBuildFrom[Set[A], B, Set[B]]): Add[B] =
      Add(f(addition))
  }

  case class Remove[A](element: A) extends SetDiffLike[A, Remove] with SetDiff[A] {
    def patch(patchee: Set[A])(implicit cbf: CanBuildFrom[Set[A], A, Set[A]]): Set[A] =
      patchee - element
    def map[B](f: A => B)(implicit cbf: CanBuildFrom[Set[A], B, Set[B]]): Remove[B] =
      Remove(f(element))
  }
}
