package hokko.collection

import hokko.core.{DBehavior, Event, IBehavior}

import collection.immutable.Vector

class IVector[A](private val underlying: IBehavior[Vector[A], SeqDelta[A]]) {
  val toIBehavior = underlying
  val toDBehavior = underlying.toDBehavior
  val toCBehavior = underlying.toDBehavior.toCBehavior

  def apply(idx: Int): DBehavior[A] = underlying.toDBehavior.map(_ apply idx)

  def updated(updates: Event[(A, Int)]): IVector[A] =
    mutation(updates) {
      case (vec, (nel, idx)) => SeqDelta.Update(vec(idx), nel, idx)
    }

  def remove(deletions: Event[Int]): IVector[A] =
    mutation(deletions) { (vec, idx) =>
      SeqDelta.Remove(vec(idx), idx)
    }

  private[this] def mutation[Arg](input: Event[Arg])(
      f: (Vector[A], Arg) => SeqDelta[A]): IVector[A] = {
    val deletionDeltas =
      underlying.toDBehavior.snapshotWith(input)(f)
    val newUnderlying =
      underlying.deltas
        .unionWith(deletionDeltas)(SeqDelta.combine)
        .fold(underlying.initial)(SeqDelta.perform)
    new IVector(newUnderlying)
  }

  def size: DBehavior[Int] =
    foldUndo(0) { (acc, _) =>
      acc + 1
    } { (acc, _) =>
      acc - 1
    }

  def count(p: A => Boolean): DBehavior[Int] =
    foldUndo(0) { (acc, n) =>
      if (p(n)) acc + 1 else acc
    } { (acc, n) =>
      if (p(n)) acc - 1 else acc
    }

  def exists(p: A => Boolean): DBehavior[Boolean] = count(p).map(_ > 0)

  def map[B](f: A => B): IVector[B] =
    new IVector(
      underlying.incMap(_ map f)(SeqDelta.mapElement(_)(f))(SeqDelta.perform))

  def foldUndo[B](init: B)(op: (B, A) => B)(undo: (B, A) => B): DBehavior[B] = {
    val foldedIBehavior = underlying.incMap(_.foldLeft(init)(op))(identity) {
      (acc, delta) =>
        SeqDelta.applyFoldUndo(acc, delta)(op)(undo)
    }
    foldedIBehavior.toDBehavior
  }

//  private def incFlatMap[B](f: A => IVector[B]): IVector[B] = {
//    val newUnderlying = Tests.incFlatMap(underlying) { vector =>
//      vector.map { a =>
//        f(a).underlying.initial
//      }.flatten
//    } { delta =>
//      val newDelta: SeqDelta[Event[SeqDelta[B]]] = SeqDelta.mapElement(delta) {
//        a =>
//          f(a).underlying.deltas
//      }
////       sequence -> Event[Delta[Delta[B]]
////       join delta -> Event[Delta[B]]
//      ???
//    }(SeqDelta.perform _)
//
//    new IVector(newUnderlying)
//  }
}

//object Tests {
//  def incFlatMap[DeltaA, A, DeltaB, B](ib: IBehavior[A, DeltaA])(fa: A => B)(
//      fb: DeltaA => Event[DeltaB])(
//      accumulator: (B, DeltaB) => B): IBehavior[B, DeltaB] = {
//
//    val newDeltas: Event[DeltaB] = ??? // ib.deltas.flatMap(fb)
//    val newInitial               = fa(ib.initial)
//
//    newDeltas.fold(newInitial)(accumulator)
//  }
//}

object IVector {
  def constant[A](vector: Vector[A]): IVector[A] =
    new IVector(IBehavior.constant(vector))
}
