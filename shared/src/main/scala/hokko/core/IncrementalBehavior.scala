package hokko.core

import scala.scalajs.js.annotation.JSExport
import scalajs2jsscala.annotation.JsScalaProxy

@JsScalaProxy
trait IncrementalBehavior[+A, +DeltaA] extends DiscreteBehavior[A] {
  val initial: A

  @JSExport
  def deltas: Event[DeltaA]

  @JSExport
  def map[B, DeltaB](accumulator: (B, DeltaB) => B)(fa: A => B)(fb: DeltaA => DeltaB): IncrementalBehavior[B, DeltaB] = {
    val newDeltas = deltas.map(fb)
    val newInitial = fa(initial)
    newDeltas.fold(newInitial)(accumulator)
  }

}

@JsScalaProxy
@JSExport
object IncrementalBehavior {
  @JSExport
  def constant[A, DeltaA](init: A): IncrementalBehavior[A, DeltaA] =
    DiscreteBehavior.constant(init).withDeltas(init, Event.empty)

  private[core] def fromDiscreteAndDeltas[A, DeltaA](init: A, db: DiscreteBehavior[A], ev: Event[DeltaA]) =
    new IncrementalBehavior[A, DeltaA] {
      val initial = init
      val node = db.node
      val changes: Event[A] = db.changes
      val deltas: Event[DeltaA] = ev
    }

  private[core] def folded[A, DeltaA](foldee: Event[DeltaA], init: A, f: (A, DeltaA) => A) = {
    val foldNode = FoldNode(foldee, init, f)
    new IncrementalBehavior[A, DeltaA] {
      val initial = init
      val node: Pull[A] = foldNode
      val changes: Event[A] = Event.fromNode(foldNode)
      val deltas: Event[DeltaA] = foldee
    }
  }

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
    def thunk(c: TickContext): Thunk[A] =
      Thunk.eager(c.getPulse(this).orElse(c.getState(this)).getOrElse(init))
  }
}
