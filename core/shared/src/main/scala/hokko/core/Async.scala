package hokko.core

import cats.effect.IO

import scala.concurrent.ExecutionContext

object Async {
  def execute[A](event: Event[IO[A]])(
      implicit ec: ExecutionContext): Event[A] = {
    val asyncNode = new Push[A] with AsyncIO[A] {
      val dependencies: List[Node[_]] = List(event.node)

      def io(context: TickContext): Option[(Push[A], IO[A])] =
        context.getPulse(event.node).map { io =>
          val shiftedIO = for {
            a <- io
            _ <- IO.shift(ec)
          } yield a

          this -> shiftedIO
        }

      def pulse(context: TickContext): Option[A] = None
    }

    Event.fromNode(asyncNode)
  }
}
