package hokko.core

import scalaz.Need

sealed trait Node[+A] {
  val dependencies: List[Node[_]]

  // lazy to give implementations the chance to fill in dependencies
  lazy val level: Int =
    if (dependencies.isEmpty) 0
    else dependencies.map(_.level).max + 1

  def updateContext(context: TickContext): Option[TickContext] = None
}

trait Pull[A] extends Node[A] {
  def thunk(context: TickContext): Need[A]

  override def updateContext(context: TickContext): Option[TickContext] = {
    val targetContext = super.updateContext(context).getOrElse(context)
    Some(targetContext.addThunk(this, thunk(targetContext)))
  }
}

trait Push[A] extends Node[A] {
  def pulse(context: TickContext): Option[A]

  override def updateContext(context: TickContext): Option[TickContext] = {
    val targetContext = super.updateContext(context).getOrElse(context)
    pulse(targetContext).map(targetContext.addPulse(this, _))
  }
}

trait State[A] extends Node[A] {
  def state(context: TickContext): Option[A]

  override def updateContext(context: TickContext): Option[TickContext] = {
    val targetContext = super.updateContext(context).getOrElse(context)
    state(targetContext).map(targetContext.addState(this, _))
  }
}
