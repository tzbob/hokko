package hokko.core

import scala.language.higherKinds

class HMap[Relation[_, _]](underlying: Map[Any, Any] = Map.empty) {
  def get[K, V](k: K)(implicit ev: Relation[K, V]): Option[V] = underlying.get(k).asInstanceOf[Option[V]]
  def +[K, V](kv: (K, V))(implicit ev: Relation[K, V]): HMap[Relation] = new HMap(underlying + kv)
  def -[K, V](k: K): HMap[Relation] = new HMap(underlying - k)
}

object HMap {
  def empty[Relation[_, _]]: HMap[Relation] = new HMap(Map.empty)
}
