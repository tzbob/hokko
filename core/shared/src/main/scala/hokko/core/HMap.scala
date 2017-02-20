package hokko.core

import scala.language.higherKinds

class HMap[Key[_], Result[_]](underlying: Map[Any, Any] = Map.empty) {
  def get[A](k: Key[A]): Option[Result[A]] =
    underlying.get(k).asInstanceOf[Option[Result[A]]]
  def +[A](kv: (Key[A], Result[A])): HMap[Key, Result] =
    new HMap(underlying + kv)
  def -[A](key: Key[A]): HMap[Key, Result] = new HMap(underlying - key)
}

object HMap {
  def empty[Key[_], Result[_]]: HMap[Key, Result] = new HMap(Map.empty)
}
