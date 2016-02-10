package hokko.core

import scala.annotation.tailrec
import scala.language.existentials
import scala.scalajs.js.annotation.JSExport
import scalajs2jsscala.annotation.JsScalaProxy

@JsScalaProxy
class Engine private (exitNodes: Seq[Node[_]]) {
  import Engine._

  private var handlers = Set.empty[Pulses => Unit]
  private[this] var memoTable = HMap.empty[TickContext.StateRelation]
  private[this] def currentContext() = TickContext.fromMemoTable(memoTable)

  private val nodeToDescendants = Engine.buildDescendants(exitNodes)
  private val orderedNodes = Engine.sortedNodes(exitNodes, nodeToDescendants)

  @JSExport
  def fire(pulses: Seq[(EventSource[A], A) forSome { type A }]): Unit =
    this.synchronized {
      val startContext = pulses.foldLeft(currentContext()) {
        // add initial pulses to the fire targets
        case (acc, (src, x)) => acc.addPulse(src.node, x)
      }
      val endContext = propagate(startContext)
      handlers.foreach { handler =>
        handler(new Pulses(this, endContext))
      }
    }

  private[this] def propagate(startContext: TickContext): TickContext = {
    val endContext = propagationResults(startContext)
    memoTable = endContext.memoTable
    endContext
  }

  private[this] def propagationResults(startContext: TickContext): TickContext =
    // TODO (if this is a bottleneck): to shortcut propagation as much as possible
    // a node's action can be divided into reactions to noisy and silent updates
    // - propagation contexts need; queuedForSilent: Node[_] => Boolean, queuedForNoisy: Node[_] => Boolean
    // - nodes need; reactToSilent(TickContext): Update[TickContext] and reactToNoisy ...
    orderedNodes.foldLeft(startContext) { (context, node) =>
      node.updateContext(context).getOrElse(context)
    }

  @JSExport
  def askCurrentValues(): Values = new Values(this, propagate(currentContext()))

  @JSExport
  def subscribeForPulses(handler: Pulses => Unit): Subscription = {
    handlers += handler
    new Subscription(this, handler)
  }
}

@JsScalaProxy
@JSExport
object Engine {
  @JsScalaProxy
  class Subscription private[Engine] (engine: Engine, handler: Pulses => Unit) {
    @JSExport
    def cancel(): Unit = engine.handlers -= handler
  }

  @JsScalaProxy
  class Values private[Engine] (engine: Engine, context: TickContext) {
    @JSExport
    def apply[A](beh: Behavior[A]): Option[A] =
      context.getThunk(beh.node).map(_.force)
  }

  @JsScalaProxy
  class Pulses private[Engine] (engine: Engine, context: TickContext) {
    @JSExport
    def apply[A](ev: Event[A]): Option[A] =
      context.getPulse(ev.node)
  }

  @JSExport
  def compile(events: Seq[Event[_]], behaviors: Seq[Behavior[_]]): Engine =
    new Engine(events.map(_.node) ++ behaviors.map(_.node))

  private[core] def buildDescendants(nodes: Seq[Node[_]]): Map[Node[_], Set[Node[_]]] = {
    @tailrec
    def buildDescendants(nodes: List[Node[_]], acc: Map[Node[_], Set[Node[_]]]): Map[Node[_], Set[Node[_]]] =
      nodes match {
        case Nil => acc
        case node :: ns =>
          val newAcc = node.dependencies.foldLeft(acc) { (map, dependency) =>
            val newDescendants = map(dependency) + node
            map + (dependency -> newDescendants)
          }
          buildDescendants(node.dependencies ++ ns, newAcc)
      }
    buildDescendants(nodes.toList, Map.empty.withDefaultValue(Set.empty))
  }

  private[core] def sortedNodes(start: Seq[Node[_]], descendants: Node[_] => Set[Node[_]]): List[Node[_]] = {
    @tailrec
    def allNodes(todo: List[Node[_]], accumulator: Set[Node[_]]): Set[Node[_]] =
      todo match {
        case Nil => accumulator
        case x :: xs => allNodes(xs ++ x.dependencies, accumulator + x)
      }
    val nodes = allNodes(start.toList, Set.empty)
    nodes.toList.sortBy(_.level)
  }
}
