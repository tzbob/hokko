package hokko.control

import cats.data.Reader
import hokko.core.Engine.Subscription
import hokko.core.{Engine, Event}

import scala.annotation.unchecked.uncheckedVariance

case class Network[+Result](
    engine: Engine,
    subscriptionMap: Map[Event[_], Subscription],
    // Reader is used safely
    private val resultReader: Engine => Result
) {
  def cancelAllSubscriptions(event: Event[_]): Unit =
    subscriptionMap.get(event).foreach(_.cancel())

  def now(): Result = resultReader(engine)
}
