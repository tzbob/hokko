package hokko.core

import hokko.core.tc.Snapshottable
import hokko.syntax.SnapshottableSyntax

trait IBehavior[+A, +DeltaA] extends DBehavior[A] {
  val initial: A
  private[core] val node: Pull[A]

  def changes: Event[A]
  def deltas: Event[DeltaA]

  def map[B, DeltaB](accumulator: (B, DeltaB) => B)(fa: A => B)(
      fb: DeltaA => DeltaB): IBehavior[B, DeltaB] = {
    val newDeltas  = deltas.map(fb)
    val newInitial = fa(initial)
    newDeltas.fold(newInitial)(accumulator)
  }

  def toDBehavior: DBehavior[A] = new DBehavior[A] {
    override private[core] val node: Pull[A] = IBehavior.this.node
    override def changes(): Event[A]         = IBehavior.this.changes
  }
}

object IBehavior extends SnapshottableSyntax {

  type IBehaviorA[A] = IBehavior[A, _]

  implicit val hokkoDBehaviorInstances: tc.Snapshottable[IBehaviorA, Event] =
    new Snapshottable[IBehaviorA, Event] {
      override def snapshotWith[A, B, C](b: IBehavior[A, Any], ev: Event[B])(
          f: (A, B) => C): Event[C] =
        Event.snapshotted(ev.map(x => (bv: A) => f(bv, x)), b)
    }

  def constant[A, DeltaA](init: A): IBehavior[A, DeltaA] =
    DBehavior.constant(init).withDeltas(init, Event.empty)

  private[core] def fromDiscreteAndDeltas[A, DeltaA](init: A,
                                                     db: DBehavior[A],
                                                     ev: Event[DeltaA]) =
    new IBehavior[A, DeltaA] {
      val initial               = init
      val node                  = db.node
      val changes: Event[A]     = db.changes
      val deltas: Event[DeltaA] = ev
    }

  private[core] def folded[A, DeltaA](foldee: Event[DeltaA],
                                      init: A,
                                      f: (A, DeltaA) => A) = {
    val foldNode = FoldNode(foldee, init, f)
    new IBehavior[A, DeltaA] {
      val initial               = init
      val node: Pull[A]         = foldNode
      val changes: Event[A]     = Event.fromNode(foldNode)
      val deltas: Event[DeltaA] = foldee
    }
  }

  private case class FoldNode[A, DeltaA](
      ev: Event[DeltaA],
      init: A,
      f: (A, DeltaA) => A
  ) extends DBehavior.PullStatePush[A] {
    val dependencies = List(ev.node)
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
}
