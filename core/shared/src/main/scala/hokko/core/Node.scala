package hokko.core

import cats.effect.IO

sealed trait Node[+A] {
  val dependencies: List[Node[_]]

  val delayed: Boolean = false

  // lazy to give implementations the chance to fill in dependencies
  lazy val level: Int =
    if (delayed || dependencies.isEmpty) 0
    else dependencies.map(_.level).max + 1

  def updateContext(context: TickContext): Option[TickContext] = None
}

trait Pull[+A] extends Node[A] {
  def thunk(context: TickContext): Thunk[A]

  override def updateContext(context: TickContext): Option[TickContext] = {
    val targetContext = super.updateContext(context).getOrElse(context)
    Some(targetContext.addThunk(this, thunk(targetContext)))
  }
}

trait Push[+A] extends Node[A] {
  def pulse(context: TickContext): Option[A]

  override def updateContext(context: TickContext): Option[TickContext] = {
    val supersContext = super.updateContext(context)
    val targetContext = supersContext.getOrElse(context)
    val pulsedContext =
      pulse(targetContext).map(targetContext.addPulse(this, _))
    pulsedContext.orElse(supersContext)
  }
}

trait AsyncIO[+A] extends Node[Nothing] {
  def io(context: TickContext): Option[(Push[A], IO[A])]

  override def updateContext(context: TickContext): Option[TickContext] = {
    io(context).map {
      case (push, io) => context.addIO(push, io)
    }
  }
}

trait State[+A] extends Node[A] {
  def state(context: TickContext): Option[A]

  override def updateContext(context: TickContext): Option[TickContext] = {
    val supersContext = super.updateContext(context)
    val targetContext = supersContext.getOrElse(context)
    val stateContext =
      state(targetContext).map(targetContext.addState(this, _))
    stateContext.orElse(supersContext)
  }
}
