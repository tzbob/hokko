package hokko.core

class TickContext private (
    pulses: HMap[Push, cats.Id],
    val memoTable: HMap[State, cats.Id],
    thunks: HMap[Pull, Thunk]
) {
  def addPulse[A](n: Push[A], pulse: A): TickContext =
    new TickContext(pulses + (n -> pulse), memoTable, thunks)
  def addState[A](n: State[A], memo: A): TickContext =
    new TickContext(pulses, memoTable + (n -> memo), thunks)
  def addThunk[A](n: Pull[A], thunk: Thunk[A]): TickContext =
    new TickContext(pulses, memoTable, thunks + (n -> thunk))

  def getPulse[A](n: Push[A]): Option[A]        = pulses.get[A](n)
  def getState[A](n: State[A]): Option[A]       = memoTable.get[A](n)
  def getThunk[A](n: Pull[A]): Option[Thunk[A]] = thunks.get(n)
}

object TickContext {
  def fromMemoTable(memoTable: HMap[State, cats.Id]): TickContext =
    new TickContext(HMap.empty, memoTable, HMap.empty)
}
