package hokko.core

import scala.language.higherKinds

trait KindMap[K[_]] {
  def get[A](k: K[A]): Option[A]
  def +[A](kv: (K[A], A)): KindMap[K]
  def -[A](k: K[A]): KindMap[K]
  def keys: collection.immutable.Set[K[_]]
}

object KindMap {
  private case class KindMapImpl[K[_]](map: Map[K[_], Any]) extends KindMap[K] {
    def -[A](k: K[A]): KindMap[K] = KindMapImpl[K](map - k)
    def +[A](kv: (K[A], A)): KindMap[K] = KindMapImpl[K](map + kv)
    def get[A](k: K[A]): Option[A] = map.get(k).map(_.asInstanceOf[A])
    def keys: Set[K[_]] = map.keySet
  }
  def empty[K[_]]: KindMap[K] = KindMapImpl[K](Map())
}
