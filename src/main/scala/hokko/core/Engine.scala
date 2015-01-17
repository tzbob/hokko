package hokko.core

import scala.annotation.tailrec
import scala.language.existentials
import scalaz.Need
import scalaz.syntax.monad._
import shapeless.HMap

class Engine private (exitNodes: Seq[Push[_]]) {
  private[this] var handlers = Set.empty[Pulses => Unit]
  private[this] var memoTable = HMap.empty[TickContext.StateRelation]
  private[this] def currentContext() = TickContext.fromMemoTable(memoTable)

  private val nodeToDescendants = Engine.buildDescendants(exitNodes)
  private val orderedNodes = Engine.sortedNodes(exitNodes, nodeToDescendants)

  def fire(pulses: (EventSource[A], A) forSome { type A }*): Unit = this.synchronized {
    val startContext = pulses.foldLeft(currentContext()) {
      case (acc, (src, x)) => acc.addPulse(src.node, x)
    }
    propagate(startContext)
  }

  private[this] def propagate(startContext: TickContext): Unit = {
    val endContext = propagationResults(startContext)
    handlers.foreach { handler =>
      handler(new Pulses(endContext))
    }
    memoTable = endContext.memoTable
  }

  private[this] def propagationResults(startContext: TickContext): TickContext =
    // TODO (if this is a bottleneck): to shortcut propagation as much as possible
    // a node's action can be divided into reactions to nosiy and silent updates
    // - propagation contexts need; queuedForSilent: Node[_] => Boolean, queuedForNoisy: Node[_] => Boolean
    // - nodes need; reactToSilent(TickContext): Update[TickContext] and reactToNoisy ...
    orderedNodes.foldLeft(startContext) { (context, node) =>
      node.updateContext(context).getOrElse(context)
    }

  def askCurrentValues(): Values = new Values(propagationResults(currentContext()))
  def subscribeForPulses[A](handler: Pulses => Unit): Subscription[A] = {
    handlers += handler
    new Subscription(handler)
  }

  class Subscription[A] private[Engine] (handler: Pulses => Unit) {
    def cancel(): Unit = handlers -= handler
  }

  class Values private[Engine] (context: TickContext) {
    def apply[A](beh: Behavior[A]): Option[Need[A]] =
      context.getThunk(beh.node)
  }

  class Pulses private[Engine] (context: TickContext) {
    def apply[A](ev: Event[A]): Option[A] =
      context.getPulse(ev.node)
  }

}

object Engine {
  def compile(events: Event[_]*): Engine = new Engine(events.map(_.node))

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
