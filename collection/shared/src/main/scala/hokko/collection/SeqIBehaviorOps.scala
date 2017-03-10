package hokko.collection

import cats.data.Ior
import hokko.collection.ICollection.ICollection
import hokko.core.{Event, IBehavior}

import scala.collection.{GenSeq, SeqLike}
import scala.collection.generic.{CanBuildFrom, IsSeqLike}

trait SeqIBehaviorOps {
  implicit class SeqIBehavior[A0, Repr[A0] <: SeqLike[A0, Repr[A0]]](
      rep: ICollection[A0, Repr[A0]])(implicit isl: IslAux[Repr[A0], A0]) {

    private def pendOperation(pend: Event[A0])(f: A0 => Delta[A0, Repr[A0]]) = {
      val prependsDelta = pend.map(f)
      val newDeltas     = Delta.combine(rep.deltas, prependsDelta)
      Delta.foldApply(rep.initial, newDeltas)
    }

    def :+(append: Event[A0])(
        implicit cbf: CanBuildFrom[Repr[A0], A0, Repr[A0]])
      : ICollection[A0, Repr[A0]] =
      pendOperation(append)(Append.apply[A0, Repr])

    def +:(prepends: Event[A0])(
        implicit cbf: CanBuildFrom[Repr[A0], A0, Repr[A0]])
      : ICollection[A0, Repr[A0]] =
      pendOperation(prepends)(Prepend.apply[A0, Repr])

    def updated(updates: Event[(Int, A0)])(
        implicit cbf: CanBuildFrom[Repr[A0], A0, Repr[A0]])
      : ICollection[A0, Repr[A0]] = {

      // map deltas and patches onto left and right
      val lefties =
        rep.deltas.map(Ior.left[Delta[A0, Repr[A0]], (Int, A0)])
      val righties =
        updates.map(Ior.right[Delta[A0, Repr[A0]], (Int, A0)])

      // combine deltas and updates into one event (simultaneous occurences
      // are merged into Ior.both
      val combined = lefties.unionWith(righties) {
        case (Ior.Left(delta), Ior.Right(prepatch)) =>
          Ior.both(delta, prepatch)
        case _ => throw new RuntimeException()
      }

      // fold combined deltas and patches into an incremental behavior that
      // tracks both the value as well as any patches that were applied
      // 1. start with (initial value, None)
      val ib: IBehavior[(Repr[A0], Option[Updated[A0, Repr]]),
                        Ior[Delta[A0, Repr[A0]], (Int, A0)]] =
        combined.fold(rep.initial -> Option.empty[Updated[A0, Repr]]) {
          (acc, ior) =>
            ior match {
              // 2. delta (left) => (apply to previous value, None)
              case Ior.Left(delta) => delta(acc._1) -> None
              // 3. new patch (right) =>
              // a) calculate removed elements
              // b) create patch delta
              // c) (apply patch, patch)
              case Ior.Right((idx, next)) =>
                val update = Updated[A0, Repr](idx, acc._1(idx), next)
                update(acc._1) -> Some(update)
              // 4. new delta and patch =>
              // repeat 3.
              // (apply patch apply delta, patch)
              case Ior.Both(delta, (idx, next)) =>
                val update = Updated[A0, Repr](idx, acc._1(idx), next)
                update(delta(acc._1)) -> Some(update)
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
  }
}
