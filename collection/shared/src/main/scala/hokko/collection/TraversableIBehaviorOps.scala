package hokko.collection

import hokko.collection.ICollection.ICollection
import hokko.core.{DBehavior, Event, IBehavior}

import scala.collection.{GenTraversable, TraversableLike}
import scala.collection.generic.CanBuildFrom

trait TraversableIBehaviorOps {
  implicit class TraversableIBehavior[A0,
  Repr[A] <: TraversableLike[A, Repr[A]]](rep: ICollection[A0, Repr[A0]]) {

    def ++(others: Event[GenTraversable[A0]])(
        implicit cbf: CanBuildFrom[Repr[A0], A0, Repr[A0]]
    ): IBehavior[Repr[A0], Delta[A0, Repr[A0]]] = {
      val appendDeltas = others.map(x => Concat[A0, Repr](x))
      val newDeltas    = Delta.combine(rep.deltas, appendDeltas)
      Delta.foldApply(rep.initial, newDeltas)
    }

    def foldUndo[B](init: B)(op: (B, A0) => B)(
        undo: (B, A0) => B): DBehavior[B] = {
      val foldedIBehavior =
        rep.incMap(_.foldLeft(init)(op))(identity) { (acc, delta) =>
          Delta.applyFoldUndo(acc, delta, op, undo)
        }
      foldedIBehavior.toDBehavior
    }

//    def filter(p: A0 => Boolean): ICollection[A0, Repr[A0]] = {
//      rep.incMap(_ filter p)(Delta.filter(_, p)) { (repr, delta) =>
//        delta.apply(repr)
//      }
//    }

    def map[B, That](f: A0 => B)(
        implicit cbf: CanBuildFrom[Repr[A0], B, Repr[B]],
        cbfDelta: CanBuildFrom[Repr[B], B, Repr[B]],
        itlThat: ItlAux[Repr[B], B]): ICollection[B, Repr[B]] = {
      rep.incMap(_ map f)(Delta.map(_, f)) { (that, delta) =>
        delta(that)
      }
    }

  }
}
