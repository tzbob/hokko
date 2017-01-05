package hokko.collection

import cats.data.Ior
import hokko.collection.ICollection.ICollection
import hokko.core.{Event, IBehavior}

import scala.collection.{GenSeq, SeqLike}
import scala.collection.generic.{CanBuildFrom, IsSeqLike}

trait SeqIBehaviorOps {
  implicit class SeqIBehavior[A0, Repr[A0] <: SeqLike[A0, Repr[A0]]](
      rep: ICollection[A0, Repr[A0]])(implicit isl: IslAux[Repr[A0], A0]) {

    def patch(patches: Event[(Int, GenSeq[A0], Int)])(
        implicit cbf: CanBuildFrom[Repr[A0], A0, Repr[A0]])
      : ICollection[A0, Repr[A0]] = {

      // map deltas and patches onto left and right
      val lefties =
        rep.deltas.map(Ior.left[Delta[A0, Repr[A0]], (Int, GenSeq[A0], Int)])
      val righties =
        patches.map(Ior.right[Delta[A0, Repr[A0]], (Int, GenSeq[A0], Int)])

      // combine deltas and patches into one event (simultaneous occurences
      // are merged into Ior.both
      val combined = lefties.unionWith(righties) {
        case (Ior.Left(delta), Ior.Right(prepatch)) =>
          Ior.both(delta, prepatch)
        case _ => throw new RuntimeException()
      }

      // fold combined deltas and patches into an incremental behavior that
      // tracks both the value as well as any patches that were applied
      // 1. start with (initial value, None)
      val ib: IBehavior[(Repr[A0], Option[Patch[A0, Repr]]),
                        Ior[Delta[A0, Repr[A0]], (Int, GenSeq[A0], Int)]] =
        combined.fold(rep.initial -> Option.empty[Patch[A0, Repr]]) {
          (acc, ior) =>
            ior match {
              // 2. delta (left) => (apply to previous value, None)
              case Ior.Left(delta) => delta(acc._1) -> None
              // 3. new patch (right) =>
              // a) calculate removed elements
              // b) create patch delta
              // c) (apply patch, patch)
              case Ior.Right((from, newElements, replaced)) =>
                val oldElements = acc._1.slice(from, from + replaced)
                val patch =
                  Patch[A0, Repr](from, newElements, replaced, oldElements)
                patch(acc._1) -> Some(patch)
              // 4. new delta and patch =>
              // repeat 3.
              // (apply patch apply delta, patch)
              case Ior.Both(delta, (from, newElements, replaced)) =>
                val oldElements = acc._1.slice(from, from + replaced)
                val patch =
                  Patch[A0, Repr](from, newElements, replaced, oldElements)
                patch(delta(acc._1)) -> Some(patch)
            }
        }

      // mold ib into the incremental behavior that we need: an incremental
      // behavior build from deltas (including the patches from ib)
      ib.incMap2(ib) { (ibS, _) =>
        ibS._1
      } {
        case (ibS, _, Ior.Both(ior, _)) =>
          ior match {
            case Ior.Left(delta)     => Some(delta)
            case Ior.Right(prepatch) => ibS._2
            case Ior.Both(delta, prepatch) =>
              ibS._2.map(Delta.combineDelta(delta, _))
          }
        case _ => throw new RuntimeException()
      } { (acc, x) =>
        // TODO: This is the second time that this value is computed, we need
        // a way to store the old value in DeltaC without exposing it
        x(acc)
      }
    }

    def :+(appends: Event[A0])(
        implicit cbf: CanBuildFrom[Repr[A0], A0, Repr[A0]])
      : ICollection[A0, Repr[A0]] = {
      val nbAppends = appends.fold(0) { (acc, _) =>
        acc + 1
      }
      val size = rep.toDBehavior.map2(nbAppends.toDBehavior)(_.length + _)

      val patches = size.snapshotWith(appends) { (size, newElement) =>
        (size, List(newElement), 0)
      }
      patch(patches)
    }

    def +:(prepends: Event[A0])(
        implicit cbf: CanBuildFrom[Repr[A0], A0, Repr[A0]])
      : ICollection[A0, Repr[A0]] =
      patch(prepends.map(x => (0, List(x), 0)))

    def updated(updates: Event[(Int, A0)])(
        implicit cbf: CanBuildFrom[Repr[A0], A0, Repr[A0]])
      : ICollection[A0, Repr[A0]] =
      patch(updates.map { case (idx, el) => (idx, List(el), 1) })
  }
}
