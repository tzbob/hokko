package hokko.core

abstract class Thunk[+A] { self =>
  def force: A
  def map[B](f: A => B): Thunk[B] = new Thunk[B] {
    def force: B = f(self.force)
  }
  def flatMap[B](f: A => Thunk[B]): Thunk[B] = new Thunk[B] {
    def force: B = f(self.force).force
  }
}

object Thunk {
  def apply[A](a: => A): Thunk[A] = new Thunk[A] {
    lazy val memo = a
    def force: A  = memo
  }

  def eager[A](a: A): Thunk[A] = new Thunk[A] {
    def force: A = a
  }
}
