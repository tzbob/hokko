package hokko.collection

import MapBehavior._
import hokko.core.{ DiscreteBehavior, Event, IncrementalBehavior }
import scala.collection.{ TraversableLike, TraversableOnce }
import scala.collection.generic.CanBuildFrom
import scala.language.{ higherKinds, implicitConversions }

class MapBehaviorOps[K, V, D[X, Y] <: MapDiffLike[X, Y, D] with MapDiff[X, Y]] private[collection] (
  val self: MapBehavior[K, V, D]
) extends AnyVal {
  def size: DiscreteBehavior[Int] = {
    val initialSize = self.initial.size
    self.deltas.fold(initialSize) { _ + _.sizeDiff }
  }

  def incrementalMapValues[B](f: V => B)(
    implicit
    cbfMap: CanBuildFrom[Map[K, V], (K, B), Map[K, B]],
    cbfId: CanBuildFrom[Map[K, B], (K, B), Map[K, B]]
  ): MapBehavior[K, B, D] = {
    val appends: Event[D[K, B]] = self.deltas.map { _.mapValue(f) }
    val newInitial = self.initial.mapValues(f)
    appends.fold(newInitial) { (acc, diff) => diff.patch(acc) }
  }

  private def selfPatch[B](evt: Event[B])(f: B => MapDiff[K, V]): MapBehavior[K, V, MapDiff] = {
    val conses = evt.map(f)
    conses.unionWith(self.deltas)(identity)(x => x: MapDiff[K, V]) { (left, right) => Merged(left, right) }
    conses.fold(self.initial) { (acc, diff) => diff.patch(acc) }
  }

  def +(kvs: Event[(K, V)])(implicit cbfId: CanBuildFrom[Map[K, V], (K, V), Map[K, V]]): MapBehavior[K, V, MapDiff] =
    selfPatch(kvs)(Add(_))
  def -(ks: Event[K])(implicit cbfId: CanBuildFrom[Map[K, V], (K, V), Map[K, V]]): MapBehavior[K, V, MapDiff] =
    selfPatch(ks)(Delete(_))
}

trait MapBehaviorSyntax {
  type MapBehavior[K, V, D[K, V] <: MapDiffLike[K, V, D]] = IncrementalBehavior[Map[K, V], D[K, V]]
}

object MapBehavior extends MapBehaviorSyntax {
  sealed trait MapDiff[K, V] extends MapDiffLike[K, V, MapDiff]

  sealed trait MapDiffLike[K, V, +This[_, _]] extends Diff[(K, V), Map[K, V]] { self: MapDiff[K, V] =>
    def sizeDiff: Int
    def mapValue[B](f: V => B)(implicit cbf: CanBuildFrom[Map[K, V], (K, B), Map[K, B]]): This[K, B]
  }

  case class Merged[K, V](diffs: MapDiff[K, V]*) extends MapDiffLike[K, V, Merged] with MapDiff[K, V] {
    val sizeDiff = diffs.map(_.sizeDiff).sum
    def patch(patchee: Map[K, V])(implicit cbf: CanBuildFrom[Map[K, V], (K, V), Map[K, V]]): Map[K, V] =
      diffs.foldLeft(patchee) { (acc, diff) => diff.patch(acc) }
    def mapValue[B](f: V => B)(implicit cbf: CanBuildFrom[Map[K, V], (K, B), Map[K, B]]): Merged[K, B] =
      Merged(diffs.map(_.mapValue(f)): _*)
  }

  case class Add[K, V](kv: (K, V)) extends MapDiffLike[K, V, Add] with MapDiff[K, V] {
    val sizeDiff = 1
    def patch(patchee: Map[K, V])(implicit cbf: CanBuildFrom[Map[K, V], (K, V), Map[K, V]]): Map[K, V] =
      patchee + kv
    def mapValue[B](f: V => B)(implicit cbf: CanBuildFrom[Map[K, V], (K, B), Map[K, B]]): Add[K, B] = {
      val (left, right) = kv
      Add(left, f(right))
    }
  }

  case class Delete[K, V](key: K) extends MapDiffLike[K, V, Delete] with MapDiff[K, V] {
    val sizeDiff = 1
    def patch(patchee: Map[K, V])(implicit cbf: CanBuildFrom[Map[K, V], (K, V), Map[K, V]]): Map[K, V] =
      patchee - key
    def mapValue[B](f: V => B)(implicit cbf: CanBuildFrom[Map[K, V], (K, B), Map[K, B]]): Delete[K, B] =
      Delete[K, B](key)
  }
}
