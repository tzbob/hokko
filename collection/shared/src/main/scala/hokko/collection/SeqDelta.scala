package hokko.collection

sealed trait SeqDelta[+A]

object SeqDelta {
  case object Empty                            extends SeqDelta[Nothing]
  case class Insert[A](element: A, index: Int) extends SeqDelta[A]
  case class Remove[A](element: A, index: Int) extends SeqDelta[A]
  case class Update[A](pElement: A, nElement: A, index: Int)
      extends SeqDelta[A]
  case class Combined[A] private (d1: SeqDelta[A], d2: SeqDelta[A])
      extends SeqDelta[A]

  def mkString(delta: SeqDelta[_], sep: String): String = delta match {
    case Empty                 => "[]"
    case Insert(el, idx)       => s"+ $el $idx$sep"
    case Remove(el, idx)       => s"- $el $idx$sep"
    case Update(pel, nel, idx) => s"| $pel $nel $idx$sep"
    case Combined(d1, d2)      => s"${mkString(d1, sep)} & ${mkString(d2, sep)}$sep"
  }

  def perform[A](vector: Vector[A], delta: SeqDelta[A]): Vector[A] =
    delta match {
      case Empty                 => vector
      case Insert(el, idx)       => vector.patch(idx, List(el), 1)
      case Remove(el, idx)       => vector.patch(idx, Nil, 1)
      case Update(pel, nel, idx) => vector.updated(idx, nel)
      case Combined(d1, d2)      => perform(perform(vector, d1), d2)
    }

  def applyFoldUndo[A, B](acc: A, d: SeqDelta[B])(op: (A, B) => A)(
      undo: (A, B) => A): A = {
    d match {
      case Empty           => acc
      case Insert(el, idx) => op(acc, el)
      case Remove(el, idx) => undo(acc, el)
      case Combined(d1, d2) =>
        applyFoldUndo(applyFoldUndo(acc, d1)(op)(undo), d2)(op)(undo)
      case Update(pel, nel, idx) =>
        op(undo(acc, pel), nel)
    }
  }

  // think about this
  def combine[A](d1: SeqDelta[A], d2: SeqDelta[A]): SeqDelta[A] =
    (d1, d2) match {
      case (Empty, x) => x
      case (x, Empty) => x
      case _          => Combined(d1, d2)
    }

//  def filter[A](f: A => Boolean)(d: SeqDelta[A]): SeqDelta[A] = d match {
//    case Empty => Empty
//    case Update(pel, nel, idx) =>
//      if (f(nel)) Update(pel, nel, idx)
//      else Remove(pel, idx)
//    case Insert(el, idx) =>
//      if (f(el)) Insert(el, idx)
//      else Remove(el, idx)
//    case Remove(el, idx) =>
//      Remove(el, idx)
//    case Combined(d1, d2) => combine(filter(f)(d), filter(f)(d))
//  }

  def map[A, B](d: SeqDelta[A])(f: (A, Int) => (B, Int)): SeqDelta[B] =
    d match {
      case Combined(d1, d2) => combine(map(d1)(f), map(d2)(f))
      case Update(pel, nel, idx) =>
        val (pel0, newIdx) = f(pel, idx)
        val (nel0, _)      = f(nel, idx)
        Update(pel0, nel0, newIdx)
      case Insert(a, idx) =>
        val (b, newIdx) = f(a, idx)
        Insert(b, newIdx)
      case Remove(a, idx) =>
        val (b, newIdx) = f(a, idx)
        Remove(b, newIdx)
      case Empty => Empty
    }

  def mapIndex[A](d: SeqDelta[A])(f: Int => Int): SeqDelta[A] =
    map(d) { (el, idx) =>
      (el, f(idx))
    }

  def mapElement[A, B](d: SeqDelta[A])(f: A => B): SeqDelta[B] =
    map(d) { (el, idx) =>
      (f(el), idx)
    }

  def translate[A](d: SeqDelta[A], sum: Int): SeqDelta[A] =
    mapIndex(d)(_ + sum)
}
