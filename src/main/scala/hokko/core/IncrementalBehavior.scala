package hokko.core

import scalaz.{Need, Value}

trait IncrementalBehavior[A, DeltaA] extends DiscreteBehavior[A] {
  def deltas: Event[DeltaA]
}

object IncrementalBehavior {
  private def fromNodeAndDeltas[A, DeltaA](n: Push[A] with Pull[A], differences: Event[DeltaA]) =
    new IncrementalBehavior[A, DeltaA] {
      val node = n
      val changes: Event[A] = Event.fromNode(n)
      val deltas: Event[DeltaA] = differences
    }

  private[core] def folded[A, DeltaA](foldee: Event[DeltaA], initial: A, f: (A, DeltaA) => A) =
    fromNodeAndDeltas(FoldNode(foldee, initial, f), foldee)

  private case class FoldNode[A, DeltaA](
    ev: Event[DeltaA],
    init: A,
    f: (A, DeltaA) => A
  ) extends DiscreteBehavior.PullStatePush[A] {
    val dependencies = List(ev.node)
    def pulse(context: TickContext): Option[A] = {
      val evPulse = context.getPulse(ev.node)
      evPulse.map { pulse =>
        val previous = context.getState(this).getOrElse(init)
        f(previous, pulse)
      }
    }
    def thunk(c: TickContext): Need[A] =
      Value(c.getPulse(this).orElse(c.getState(this)).getOrElse(init))
  }
}
