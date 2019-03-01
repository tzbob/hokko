package hokko.core

import cats.data.Ior
import hokko.core.tc.Snapshottable
import hokko.syntax.{SnapshottableOps, SnapshottableSyntax}

trait IBehavior[A, DeltaA] extends Primitive[A] {
  val initial: A
  val accumulator: (A, DeltaA) => A
  override private[core] val node: Pull[A]

  def changes: Event[A]
  def deltas: Event[DeltaA]

  def incMap[B, DeltaB](fa: A => B)(fb: DeltaA => DeltaB)(
      accumulator: (B, DeltaB) => B): IBehavior[B, DeltaB] = {
    val newDeltas  = deltas.map(fb)
    val newInitial = fa(initial)
    newDeltas.foldI(newInitial)(accumulator)
  }

  // TODO: this should be incMap
  def incMapS[B, DeltaB](fa: A => B)(fb: (A, DeltaA) => DeltaB)(
      accumulator: (B, DeltaB) => B): IBehavior[B, DeltaB] = {
    val newDeltas  = this.snapshotWith(this.deltas)(fb)
    val newInitial = fa(initial)
    newDeltas.foldI(newInitial)(accumulator)
  }

  def incMap2[B, DeltaB, C, DeltaC](b: IBehavior[B, DeltaB])(
      valueFun: (A, B) => C)(
      deltaFun: (A, B, Ior[DeltaA, DeltaB]) => Option[DeltaC])(
      foldFun: (C, DeltaC) => C
  ): IBehavior[C, DeltaC] = {
    val newInit = valueFun(initial, b.initial)

    val newDelta: Event[DeltaC] = {
      val abs = this.toDBehavior.map2(b.toDBehavior) { (_, _) }

      val lefts  = this.deltas.map(Ior.left[DeltaA, DeltaB])
      val rights = b.deltas.map(Ior.right[DeltaA, DeltaB])
      val increments = lefts.unionWith(rights) { (l, r) =>
        Ior.both(l.left.get, r.right.get) // safe because l is always left, r is always right
      }

      val tupled = abs.snapshotWith(increments) {
        case ((a, b), inc) => (a, b, inc)
      }
      tupled.collect(deltaFun.tupled)
    }

    newDelta.foldI(newInit)(foldFun)
  }

  def toDBehavior[AA >: A]: DBehavior[AA] = new DBehavior[AA] {
    val init                                 = IBehavior.this.initial
    override private[core] val node: Pull[A] = IBehavior.this.node
    override def changes(): Event[A]         = IBehavior.this.changes
  }

  def toCBehavior[AA >: A]: CBehavior[AA] = toDBehavior.toCBehavior
}

object IBehavior {
  implicit def syntaxSnapshottable[A, DA](b: IBehavior[A, DA])(
      implicit ev: Snapshottable[IBehavior[?, DA], Event])
    : SnapshottableOps[IBehavior[?, DA], A] =
    new SnapshottableOps[IBehavior[?, DA], A](b)

  implicit def hokkoIBehaviorInstances[D]
    : tc.Snapshottable[IBehavior[?, D], Event] =
    new Snapshottable[IBehavior[?, D], Event] {
      def snapshotWith[A, B, C](b: IBehavior[A, D], ev: Event[B])(
          f: (A, B) => C): Event[C] =
        Event.snapshotted(ev.map(x => (bv: A) => f(bv, x)), b)
    }

  def constant[A, DeltaA](init: A): IBehavior[A, DeltaA] =
    Event.empty[DeltaA].foldI(init) { (acc, _) =>
      acc
    }

  private[core] def folded[A, DeltaA](foldee: Event[DeltaA],
                                      init: A,
                                      f: (A, DeltaA) => A) = {
    val foldNode = new FoldNode(foldee, init, f)
    new IBehavior[A, DeltaA] {
      val initial                       = init
      val node: Pull[A]                 = foldNode
      val changes: Event[A]             = Event.fromNode(foldNode)
      val deltas: Event[DeltaA]         = foldee
      val accumulator: (A, DeltaA) => A = f
    }
  }

  private class FoldNode[A, DeltaA](
      ev: Event[DeltaA],
      init: A,
      f: (A, DeltaA) => A
  ) extends DBehavior.PullStatePush[A] {
    val dependencies: List[Node[_]] = List(ev.node)
    def pulse(context: TickContext): Option[A] = {
      val evPulse = context.getPulse(ev.node)
      evPulse.map { pulse =>
        val previous = context.getState(this).getOrElse(init)
        f(previous, pulse)
      }
    }
    def thunk(c: TickContext): Thunk[A] =
      Thunk.eager(c.getPulse(this).orElse(c.getState(this)).getOrElse(init))
  }

  private[core] def resetFolded[A, DeltaA](foldee: Event[DeltaA],
                                           resetter: Event[A],
                                           init: A,
                                           f: (A, DeltaA) => A) = {
    val foldNode = ResetFoldNode(foldee, resetter, init, f)
    new IBehavior[A, DeltaA] {
      val initial                       = init
      val node: Pull[A]                 = foldNode
      val changes: Event[A]             = Event.fromNode(foldNode)
      val deltas: Event[DeltaA]         = foldee
      val accumulator: (A, DeltaA) => A = f
    }
  }

  private case class ResetFoldNode[A, DeltaA](
      ev: Event[DeltaA],
      resetter: Event[A],
      init: A,
      f: (A, DeltaA) => A
  ) extends FoldNode[A, DeltaA](ev, init, f) {
    override val dependencies = List(ev.node, resetter.node)

    // If the resetter produces a pulse, reset the state to that pulse
    override def state(context: TickContext): Option[A] =
      context.getPulse(resetter.node).orElse(super.state(context))
  }

}
