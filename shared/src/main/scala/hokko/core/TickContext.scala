package hokko.core

class TickContext private (
  pulses: HMap[TickContext.PushRelation],
  val memoTable: HMap[TickContext.StateRelation],
  thunks: HMap[TickContext.PullRelation]
) {
  implicit def pushRel[A] = new TickContext.PushRelation[Push[A], A]
  implicit def stateRel[A] = new TickContext.StateRelation[State[A], A]
  implicit def pullRel[A] = new TickContext.PullRelation[Pull[A], Thunk[A]]

  def addPulse[A](n: Push[A], pulse: A): TickContext =
    new TickContext(pulses + (n -> pulse), memoTable, thunks)
  def addState[A](n: State[A], memo: A): TickContext =
    new TickContext(pulses, memoTable + (n -> memo), thunks)
  def addThunk[A](n: Pull[A], thunk: Thunk[A]): TickContext =
    new TickContext(pulses, memoTable, thunks + (n -> thunk))

  def getPulse[A](n: Push[A]): Option[A] = pulses.get(n)
  def getState[A](n: State[A]): Option[A] = memoTable.get(n)
  def getThunk[A](n: Pull[A]): Option[Thunk[A]] = thunks.get(n)
}

object TickContext {
  class PushRelation[K, V]
  class StateRelation[K, V]
  class PullRelation[K, V]

  def fromMemoTable(memoTable: HMap[StateRelation]): TickContext =
    new TickContext(HMap.empty, memoTable, HMap.empty)
}

