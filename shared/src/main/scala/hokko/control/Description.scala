package hokko.control

import cats.Applicative
import cats.data.Reader
import cats.syntax.{ApplySyntax, FunctorSyntax}
import hokko.core._

object Description extends FunctorSyntax with ApplySyntax {
  implicit val networkApplicativeInstance: Applicative[Description] =
    new Applicative[Description] {
      def pure[A](x: A): Description[A] =
        Description(Set.empty, Seq.empty, Reader((v: Engine.Values) => x))

      def ap[A, B](ff: Description[A => B])(
          fa: Description[A]): Description[B] =
        Description(
          ff.deps ++ fa.deps,
          ff.handlers ++ fa.handlers,
          fa.resultReader ap ff.resultReader
        )
    }

  def listen[A](b: CBehavior[A]): Description[A] =
    Description(
      Set(b),
      Seq.empty,
      Reader((v: Engine.Values) => v(b).get)
    )
}

case class Description[Result](
    private val deps: Set[Primitive[_]],
    private val handlers: Seq[Handler[_]],
    private val resultReader: Reader[Engine.Values, Result]) {
  import cats.syntax.apply._

  def subscribe[A](event: Event[A])(handler: A => Unit): Description[Result] =
    this.copy(deps = this.deps + event,
              handlers = this.handlers :+ Handler(event, handler))

  def listen[A, B](b: CBehavior[A])(f: (Result, A) => B): Description[B] = {
    val newDeps = this.deps + b

    val readB           = Reader((v: Engine.Values) => v(b).get)
    val newResultReader = resultReader.map2(readB)(f)

    this.copy(resultReader = newResultReader, deps = newDeps)
  }

  def compile(): Network[Result] = {
    val engine = Engine.compile(deps.toList)
    val eventsToSubscription = handlers.map { handler =>
      handler.e -> engine.subscribeForPulses(handler.pulseFun)
    }

    Network(engine, eventsToSubscription.toMap, resultReader)
  }
}
