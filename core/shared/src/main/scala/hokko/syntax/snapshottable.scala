package hokko.syntax

import hokko.core.tc.Snapshottable

trait SnapshottableSyntax[Beh[_]] {
  implicit def syntaxSnapshottable[A](b: Beh[A]): SnapshottableOps[Beh, A] =
    new SnapshottableOps[Beh, A](b)
}

final class SnapshottableOps[Beh[_], A](b: Beh[A]) {
  def snapshotWith[B, C, Ev[_]](e: Ev[B])(f: (A, B) => C)(
      implicit
      ev: Snapshottable[Beh, Ev]): Ev[C] =
    ev.snapshotWith(b, e)(f)

  def sampledBy[Ev[_]](e: Ev[_])(implicit ev: Snapshottable[Beh, Ev]): Ev[A] =
    ev.sampledBy(b, e)
}
