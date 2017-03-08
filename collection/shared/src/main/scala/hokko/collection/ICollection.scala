package hokko.collection

import hokko.core.{Event, IBehavior}

import scala.collection.generic.{CanBuildFrom, IsTraversableLike}

object ICollection {
  type ICollection[El, Repr] = IBehavior[Repr, Delta[El, Repr]]

  def constant[Repr](v: Repr)(
      implicit itl: IsTraversableLike[Repr]): ICollection[itl.A, Repr] =
    IBehavior.constant[Repr, Delta[itl.A, Repr]](v)

  def empty[Repr](implicit itl: IsTraversableLike[Repr],
                  cbf: CanBuildFrom[Repr, _, Repr]): ICollection[itl.A, Repr] =
    IBehavior.constant[Repr, Delta[itl.A, Repr]](cbf.apply().result())

  object implicits extends TraversableIBehaviorOps with SeqIBehaviorOps
}
