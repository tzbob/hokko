package hokko.collection

import scala.collection.{TraversableLike, TraversableOnce}
import scala.collection.generic.CanBuildFrom

trait Diff[A, Patchee] {
  def patch(patchee: Patchee)(implicit cbf: CanBuildFrom[Patchee, A, Patchee]): Patchee
}
