package hokko.core

import cats.Applicative
import cats.syntax.{ApplicativeSyntax, ApplySyntax, FunctorSyntax}
import hokko.syntax.SnapshottableSyntax
import slogging.LazyLogging

trait DBehavior[A] extends Primitive[A] {
  override private[core] val node: Pull[A]
  val init: A

  def changes(): Event[A]

  def toCBehavior[AA >: A]: CBehavior[A] = new CBehavior[A] {
    override private[core] val node: Pull[A] = DBehavior.this.node
    val initial: Thunk[A]                    = Thunk.eager(DBehavior.this.init)
  }

  /**
  *
    * @param diff computes difference, first argument is new value, second is
    *             old
    * @param patch
    * @tparam DeltaA
    * @return
    */
  def toIBehavior[DeltaA](diff: (A, A) => DeltaA)(
      patch: (A, DeltaA) => A): IBehavior[A, DeltaA] = {
    val memorizedDeltas = changes.fold(Option.empty[DeltaA] -> init) {
      case ((_, oldValue), newValue) =>
        Some(diff(newValue, oldValue)) -> newValue
    }

    val diffs = memorizedDeltas.changes.collect(_._1)
    diffs.fold(init)(patch)
  }

}

object DBehavior
    extends ApplicativeSyntax
    with ApplySyntax
    with FunctorSyntax
    with SnapshottableSyntax[DBehavior]
    with LazyLogging {
  implicit val hokkoDBehaviorInstances
    : tc.Snapshottable[DBehavior, Event] with Applicative[DBehavior] =
    new tc.Snapshottable[DBehavior, Event] with Applicative[DBehavior] {
      def pure[A](x: A): DBehavior[A] = constant(x)
      def ap[A, B](ff: DBehavior[A => B])(fa: DBehavior[A]): DBehavior[B] =
        DBehavior.this.ap(ff)(fa)
      def snapshotWith[A, B, C](b: DBehavior[A], ev: Event[B])(
          f: (A, B) => C): Event[C] =
        Event.snapshotted(ev.map(x => (bv: A) => f(bv, x)), b)
    }

  def constant[A](init: A): DBehavior[A] =
    fromNode(init, ConstantNode(init))

  def ap[A, B](ff: DBehavior[A => B])(fa: DBehavior[A]): DBehavior[B] =
    fromNode(ff.init(fa.init), ReverseApply(fa, ff))

  def delayed[A](target: => DBehavior[A]): CBehavior[A] = {
    val state = new State[A] {
      lazy val dependencies: List[Node[_]] = List(target.changes.node)

      def state(context: TickContext): Option[A] = {
        context.getPulse(target.changes().node)
      }
    }

    val pull = new Pull[A] {
      // level is early so early state is pulled
      override val delayed  = true
      lazy val dependencies = List(target.node, state)

      // retrieve previous state
      def thunk(context: TickContext): Thunk[A] =
        Thunk.eager(context.getState(state).getOrElse(target.init))
    }

    new CBehavior[A] {
      override private[core] val node = pull
      val initial: Thunk[A]           = Thunk(target.init)
    }
  }

  // Convenience traits stacked in the right order
  private[core] trait PushState[A] extends Push[A] with State[A] {
    def state(context: TickContext): Option[A] =
      context.getPulse(this)
  }
  private[core] trait PullStatePush[A] extends PushState[A] with Pull[A]

  private[core] def fromNode[A](initial: => A, n: => Push[A] with Pull[A]) =
    new DBehavior[A] {
      lazy val init              = initial
      lazy val node              = n
      lazy val changes: Event[A] = Event.fromNode(n)
    }

  // primitive node implementations
  private case class ConstantNode[A](init: A) extends Push[A] with Pull[A] {
    val dependencies                           = List.empty
    def pulse(context: TickContext): Option[A] = None
    def thunk(context: TickContext): Thunk[A]  = Thunk(init)
  }

  private[core] case class Snapshot[A, B, C](
      cb: CBehavior[A],
      db: DBehavior[B],
      f: (A, B) => C
  ) extends PullStatePush[C] {
    val dependencies: List[Node[_]] = List(cb.node, db.node, db.changes().node)
    def pulse(context: TickContext): Option[C] = {
      context.getPulse(db.changes.node).map { p =>
        val dbT = context.getThunk(db.node).get
        val cbT = context.getThunk(cb.node).get

        val thunk = for {
          a <- cbT
          b <- dbT
        } yield f(a, b)

        thunk.force
      }
    }

    def thunk(context: TickContext): Thunk[C] =
      context
        .getState(this)
        .map(Thunk.eager)
        .getOrElse(cb.initial.map(a => f(a, db.init)))
  }

  private case class ReverseApply[A, B](
      apParam: DBehavior[A],
      apFun: DBehavior[A => B]
  ) extends PullStatePush[B] {
    val dependencies = List(apParam.node, apFun.node)

    def pulse(context: TickContext): Option[B] = {
      val paramPulse = context.getPulse(apParam.changes().node)
      val funPulse   = context.getPulse(apFun.changes().node)

      val paramFunOpt: Option[(A, A => B)] = (paramPulse, funPulse) match {
        case (Some(param), Some(fun)) => Some((param, fun))
        case (None, Some(fun)) =>
          val optParamThunk = context.getThunk(apParam.node)
          optParamThunk.map(th => th.force -> fun)
        case (Some(param), None) =>
          val optFunThunk = context.getThunk(apFun.node)
          optFunThunk.map(th => param -> th.force)
        case (None, None) =>
          logger.trace(s"No pulses found for $apParam and $apFun")
          None
      }

      paramFunOpt.map {
        case (param, fun) => fun(param)
      }
    }

    def thunk(c: TickContext): Thunk[B] =
      Thunk.eager(c.getState(this).getOrElse(apFun.init(apParam.init)))
  }

}
