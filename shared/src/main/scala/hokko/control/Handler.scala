package hokko.control

import hokko.core.{Engine, Event}

case class Handler[A](e: Event[A], handler: (Engine, A) => Unit) {
  val pulseFun = (engine: Engine) =>
    (p: Engine.Pulses) => {
      val p1 = p(e)
      p1.foreach { aValue =>
        handler(engine, aValue)
      }
  }
}
