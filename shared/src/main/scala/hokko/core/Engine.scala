package hokko.core

import scala.annotation.tailrec
import scala.language.existentials

class Engine private (exitNodes: Seq[Node[_]]) {
  import Engine._

  private var handlers               = Set.empty[Pulses => Unit]
  private[this] var memoTable        = HMap.empty[State, cats.Id]
  private[this] def currentContext() = TickContext.fromMemoTable(memoTable)

  private val nodeToDescendants = Engine.buildDescendants(exitNodes)
  private val orderedNodes      = Engine.sortedNodes(exitNodes, nodeToDescendants)

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

  private[this] def propagationResults(
      startContext: TickContext): TickContext =
    orderedNodes.foldLeft(startContext) { (context, node) =>
      node.updateContext(context).getOrElse(context)
    }

  def askCurrentValues(): Values =
    new Values(this, propagate(currentContext()))

  def subscribeForPulses(handler: Pulses => Unit): Subscription = {
    handlers += handler
    new Subscription(this, handler)
  }
}

object Engine {
  class Subscription private[Engine] (engine: Engine, handler: Pulses => Unit) {
    def cancel(): Unit = engine.handlers -= handler
  }

  class Values private[Engine] (engine: Engine, context: TickContext) {
    def apply[A](beh: CBehavior[A]): Option[A] =
      context.getThunk(beh.node).map(_.force)
  }

  class Pulses private[Engine] (engine: Engine, context: TickContext) {
    def apply[A](ev: Event[A]): Option[A] =
      context.getPulse(ev.node)
  }

  def compile(primitives: List[Primitive[_]]): Engine =
    new Engine(primitives.map(_.node))

  def compile(primitives: Primitive[_]*): Engine =
    new Engine(primitives.map(_.node))

  private[core] def buildDescendants(
      nodes: Seq[Node[_]]): Map[Node[_], Set[Node[_]]] = {
    @tailrec
    def buildDescendants(
        nodes: List[Node[_]],
        acc: Map[Node[_], Set[Node[_]]]): Map[Node[_], Set[Node[_]]] =
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

  private[core] def sortedNodes(
      start: Seq[Node[_]],
      descendants: Node[_] => Set[Node[_]]): List[Node[_]] = {
    @tailrec
    def allNodes(todo: List[Node[_]],
                 accumulator: Set[Node[_]]): Set[Node[_]] =
      todo match {
        case Nil     => accumulator
        case x :: xs => allNodes(xs ++ x.dependencies, accumulator + x)
      }
    val nodes = allNodes(start.toList, Set.empty)
    nodes.toList.sortBy(_.level)
  }
}
