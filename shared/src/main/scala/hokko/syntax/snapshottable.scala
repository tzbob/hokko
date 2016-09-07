package hokko.syntax

import hokko.core.tc.Snapshottable

trait SnapshottableSyntax {
  implicit def syntaxSnapshottable[A, Beh[_], Ev[_]](b: Beh[A])(
      implicit ev: Snapshottable[Beh, Ev]): SnapshottableOps[Beh, Ev, A] =
    new SnapshottableOps[Beh, Ev, A](b)
}

final class SnapshottableOps[Beh[_], Ev[_], A](b: Beh[A])(
    implicit ev: Snapshottable[Beh, Ev]) {
  def snapshotWith[B, C](e: Ev[B])(f: (A, B) => C): Ev[C] =
    ev.snapshotWith(b, e)(f)

  def sampledBy(e: Ev[_]): Ev[A] =
    ev.sampledBy(b, e)
}
