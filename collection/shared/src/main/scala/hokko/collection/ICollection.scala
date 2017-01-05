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

  object implicits extends TraversableIBehaviorOps with SeqIBehaviorOps {
//
//    val test                                 = ICollection.empty[Vector[Int]]
//    val testo                                = ICollection.empty[Set[Int]]
//    val concat: Event[List[Int]]             = ???
//    val insert: Event[(Int, List[Int], Int)] = ???
//
//    testo ++ concat
//    test.patch(insert)
  }

}
