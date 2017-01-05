package hokko

import scala.collection.generic.{IsSeqLike, IsTraversableLike}

package object collection {
  type ItlAux[Repr, A0] = IsTraversableLike[Repr] { type A = A0 }
  type IslAux[Repr, A0] = IsSeqLike[Repr] { type A = A0 }
}
