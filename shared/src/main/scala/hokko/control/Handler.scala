package hokko.control

import hokko.core.{Engine, Event}

case class Handler[A](e: Event[A], handler: A => Unit) {
  val pulseFun = (p: Engine.Pulses) => {
    val p1 = p(e)
    p1.foreach(handler)
  }
}
