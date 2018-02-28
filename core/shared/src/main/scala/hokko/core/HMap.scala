package hokko.core

import scala.language.higherKinds
import scala.language.existentials

class HMap[Key[_], Result[_]](underlying: Map[Any, Any] = Map.empty) {
  def get[A](k: Key[A]): Option[Result[A]] =
    underlying.get(k).asInstanceOf[Option[Result[A]]]
  def +[A](kv: (Key[A], Result[A])): HMap[Key, Result] =
    new HMap(underlying + kv)
  def -[A](key: Key[A]): HMap[Key, Result] = new HMap(underlying - key)

  def map[B](f: (((Key[A], Result[A])) forSome { type A }) => B): Seq[B] =
    underlying.map { kv =>
      f(kv.asInstanceOf[(((Key[A], Result[A])) forSome { type A })])
    }.toSeq
}

object HMap {
  def empty[Key[_], Result[_]]: HMap[Key, Result] = new HMap(Map.empty)
}
