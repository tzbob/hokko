package hokko.core

import cats.effect.IO
import slogging.LazyLogging

import scala.annotation.tailrec
import scala.concurrent.Future
import scala.language.existentials

class Engine private (exitNodes: Seq[Node[_]]) extends LazyLogging {
  import Engine._

  private var handlers               = Set.empty[Pulses => Unit]
  private[this] var memoTable        = HMap.empty[State, cats.Id]
  private[this] def currentContext() = TickContext.fromMemoTable(memoTable)

  private val orderedNodes = {
    val nodes = Engine.sortedNodes(exitNodes)
    logger.trace(s"Sorted nodes $nodes")
    nodes
  }

  def fire(pulses: Seq[(EventSource[A], A) forSome { type A }]): FireResult =
    fireNodes(pulses.map {
      case (src, a) => (src.node, a)
    })

  private[this] def fireNodes(
      pulses: Seq[(Push[A], A) forSome { type A }]): FireResult =
    this.synchronized {
      logger.trace(s"Firing pulses $pulses")
      val startContext = pulses.foldLeft(currentContext()) {
        // add initial pulses to the fire targets
        case (acc, (node, x)) => acc.addPulse(node, x)
      }

      val endContext = propagationResults(startContext)
      memoTable = endContext.memoTable

      logger.trace(s"Pulses callback for ${endContext.pulses.underlying}")
      handlers.foreach { handler =>
        handler(new Pulses(endContext))
      }

      val ioPropagations: Seq[IO[Engine.FireResult]] =
        endContext.asyncIOs.map {
          case (push, io) =>
            for {
              a <- io
            } yield fireNodes(Seq(push -> a))
        }

      val futurePropagations = ioPropagations.map { io =>
        io.unsafeToFuture()
      }

      FireResult(endContext, futurePropagations)
    }

  private[this] def propagationResults(startContext: TickContext): TickContext =
    orderedNodes.foldLeft(startContext) { (context, node) =>
      node.updateContext(context).getOrElse(context)
    }

  def askCurrentValues(): Values =
    new Values(propagationResults(currentContext()))

  def subscribeForPulses(handler: Pulses => Unit): Subscription = {
    handlers += handler
    new Subscription(this, handler)
  }
}

object Engine {
  case class FireResult(context: TickContext,
                        futurePropagations: Seq[Future[FireResult]]) {
    val values: Values = new Values(context)
    val Pulses: Pulses = new Pulses(context)
  }

  class Subscription private[Engine] (engine: Engine, handler: Pulses => Unit) {
    def cancel(): Unit = engine.handlers -= handler
  }

  class Values private[Engine] (context: TickContext) {
    def apply[A](beh: CBehavior[A]): Option[A] =
      context.getThunk(beh.node).map(_.force)
  }

  class Pulses private[Engine] (context: TickContext) {
    def apply[A](ev: Event[A]): Option[A] =
      context.getPulse(ev.node)
  }

  def compile(primitives: List[Primitive[_]]): Engine =
    new Engine(primitives.map(_.node))

  def compile(primitives: Primitive[_]*): Engine =
    new Engine(primitives.map(_.node))

  private[core] def sortedNodes(start: Seq[Node[_]]): List[Node[_]] = {
    @tailrec
    def allNodes(todo: List[Node[_]], accumulator: Set[Node[_]]): Set[Node[_]] =
      todo match {
        case Nil => accumulator
        case x :: xs =>
          val rest = if (accumulator contains x) xs else xs ++ x.dependencies
          allNodes(rest, accumulator + x)
      }
    val nodes = allNodes(start.toList, Set.empty)
    nodes.toList.sortBy(_.level)
  }
}
