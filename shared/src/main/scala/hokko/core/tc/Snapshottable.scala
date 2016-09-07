package hokko.core.tc

trait Snapshottable[Beh[_], Ev[_]] {
  def snapshotWith[A, B, C](b: Beh[A], ev: Ev[B])(f: (A, B) => C): Ev[C]

  def sampledBy[A](b: Beh[A], ev: Ev[_]): Ev[A] =
    snapshotWith(b, ev) { (bv, _) =>
      bv
    }
}
