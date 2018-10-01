package hokko.core
import cats.Applicative
import cats.syntax.{ApplicativeSyntax, ApplySyntax, FunctorSyntax}

abstract class Thunk[+A] { self =>
  def force: A
  def map[B](f: A => B): Thunk[B] = new Thunk[B] {
    def force: B = f(self.force)
  }
  def flatMap[B](f: A => Thunk[B]): Thunk[B] = new Thunk[B] {
    def force: B = f(self.force).force
  }
}

object Thunk extends ApplySyntax with FunctorSyntax with ApplicativeSyntax {

  implicit val thunkInstances = new Applicative[Thunk] {
    def pure[A](x: A): Thunk[A] = Thunk.eager(x)
    def ap[A, B](ff: Thunk[A => B])(fa: Thunk[A]): Thunk[B] =
      for {
        f <- ff
        a <- fa
      } yield f(a)
  }

  def apply[A](a: => A): Thunk[A] = new Thunk[A] {
    lazy val memo = a
    def force: A  = memo
  }

  def eager[A](a: A): Thunk[A] = new Thunk[A] {
    def force: A = a
  }
}
