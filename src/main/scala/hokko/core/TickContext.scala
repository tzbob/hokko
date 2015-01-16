package hokko.core

import scalaz.Need
import shapeless._

class TickContext(
  pulses: KindMap[Push],
  val memoTable: KindMap[State],
  thunks: HMap[(Pull ~?> Need)#λ]
) {
  def addPulse[A](n: Push[A], pulse: A): TickContext =
    new TickContext(pulses + (n -> pulse), memoTable, thunks)
  def addState[A](n: State[A], memo: A): TickContext =
    new TickContext(pulses, memoTable + (n -> memo), thunks)
  def addThunk[A](n: Pull[A], thunk: Need[A]): TickContext =
    new TickContext(pulses, memoTable, thunks + (n, thunk))
  def getPulse[A](n: Push[A]): Option[A] = pulses.get(n)
  def getState[A](n: State[A]): Option[A] = memoTable.get(n)
  def getThunk[A](n: Pull[A]): Option[Need[A]] = {
    implicit val test = (~?>).rel[Pull, Need]
    implicit val test2 = (~?>).witness[Pull, Need, A]
    thunks.get(n)
  }

  def pulsers: Set[Push[_]] = pulses.keys
}

object TickContext {
  def empty: TickContext = fromMemoTable(KindMap.empty)

  def fromMemoTable(memoTable: KindMap[State]): TickContext =
    new TickContext(KindMap.empty, memoTable, HMap.empty)
}

