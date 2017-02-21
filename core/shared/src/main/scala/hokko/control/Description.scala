package hokko.control

import cats.Applicative
import cats.instances.all._
import cats.syntax.all._
import cats.syntax.{ApplySyntax, FunctorSyntax}
import hokko.core._

object Description extends FunctorSyntax with ApplySyntax {
  implicit val networkApplicativeInstance: Applicative[Description] =
    new Applicative[Description] {
      def pure[A](x: A): Description[A] = Description.pure(x)

      def ap[A, B](ff: Description[A => B])(
          fa: Description[A]): Description[B] =
        Description(
          ff.deps ++ fa.deps,
          ff.handlers ++ fa.handlers,
          ff.resultReader ap fa.resultReader
        )
    }

  def subscribeEngine[A](event: Event[A])(
      handler: (Engine, A) => Unit): Description[Unit] =
    Description.pure(()).subscribeEngine(event)(handler)

  def subscribe[A](event: Event[A])(handler: A => Unit): Description[Unit] =
    subscribeEngine(event)((_, a) => handler(a))

  def read[A](b: CBehavior[A]): Description[A] =
    Description.pure(()).read(b) { (_, a) =>
      a
    }

  def pure[A](a: A): Description[A] =
    Description(Set.empty, Seq.empty, _ => a)
}

case class Description[+Result](private val deps: Set[Primitive[_]],
                                private val handlers: Seq[Handler[_]],
                                // Reader is used safely
                                private val resultReader: Engine => Result) {

  def subscribeEngine[A](event: Event[A])(
      handler: (Engine, A) => Unit): Description[Result] =
    this.copy(deps = this.deps + event,
              handlers = this.handlers :+ Handler(event, handler))

  def subscribe[A](event: Event[A])(handler: A => Unit): Description[Result] = {
    def wrapped(e: Engine, a: A) = handler(a)
    subscribeEngine(event)(wrapped)
  }

  def read[A, B](b: CBehavior[A])(f: (Result, A) => B): Description[B] = {
    val newDeps = this.deps + b

    // Get will be safe since b is added to deps
    val readB           = (v: Engine) => v.askCurrentValues()(b).get
    val newResultReader = resultReader.map2(readB)(f)

    this.copy(resultReader = newResultReader, deps = newDeps)
  }

  def mapEngine[B](f: (Engine, Result) => B): Description[B] = {
    val readE     = identity[Engine] _
    val newReader = readE.map2(resultReader)(f)
    this.copy(resultReader = newReader)
  }

  def compile(): Network[Result] = {
    val engine = Engine.compile(deps.toList)
    val eventsToSubscription = handlers.map { handler =>
      handler.e -> engine.subscribeForPulses(handler.pulseFun(engine))
    }

    Network(engine, eventsToSubscription.toMap, resultReader)
  }
}
