package hokko.core

import scalaz.Need

sealed trait Node[+A] {
  val dependencies: List[Node[_]]
  val level: Int =
    if (dependencies.isEmpty) 0
    else dependencies.map(_.level).max + 1

  def updateContext(context: TickContext): Option[TickContext] =
    None
}

trait Pull[A] extends Node[A] {
  def thunk(context: TickContext): Need[A]

  override def updateContext(context: TickContext): Option[TickContext] =
    super.updateContext(context).map(_.addThunk(this, thunk(context)))
}

trait Push[A] extends Node[A] {
  def pulse(context: TickContext): Option[A]

  override def updateContext(context: TickContext): Option[TickContext] =
    for {
      newContext <- super.updateContext(context)
      p <- pulse(newContext)
    } yield newContext.addPulse(this, p)
}

trait State[A] extends Node[A] {
  def state(context: TickContext): Option[A]

  override def updateContext(context: TickContext): Option[TickContext] =
    for {
      newContext <- super.updateContext(context)
      s <- state(context)
    } yield newContext.addState(this, s)
}
