package hokko.core

trait Primitive[+A] {
  private[core] val node: Node[A]
}
