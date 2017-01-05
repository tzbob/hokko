package hokko.control

import cats.data.Reader
import hokko.core.Engine.Subscription
import hokko.core.{Engine, Event}

case class Network[Result](
    engine: Engine,
    subscriptionMap: Map[Event[_], Subscription],
    private val resultReader: Reader[Engine.Values, Result]
) {
  def cancelAllSubscriptions(event: Event[_]): Unit =
    subscriptionMap.get(event).foreach(_.cancel())

  def now(): Result = resultReader.run(engine.askCurrentValues())
}
