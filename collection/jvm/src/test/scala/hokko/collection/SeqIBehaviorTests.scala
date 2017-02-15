package hokko.collection

import hokko.collection.ICollection.ICollection
import hokko.collection.ICollection.implicits._
import hokko.core.{Engine, Event, EventSource}
import org.scalacheck._

trait SeqIBehaviorTests extends TraversableIBehaviorTests {

  def arbitraryUpdated[T: Arbitrary]: Arbitrary[(List[T], List[(Int, T)])] = {
    def pulses(size: Int): Gen[List[(Int, T)]] = {
      val tupleGen = for {
        index <- Gen.choose(0, size)
        t     <- Arbitrary.arbitrary[T]
      } yield index -> t
      Gen.listOf(tupleGen)
    }

    val genUpdated = for {
      list    <- Gen.nonEmptyListOf(Arbitrary.arbitrary[T])
      updates <- pulses(list.length - 1)
    } yield (list, updates)

    Arbitrary(genUpdated)
  }

  def append[A](pulses: List[A]): ICollectionTransformation[A, List[A]] = {
    val src = Event.source[A]

    ICollectionTransformation[A, List[A]](
      ic => {
        val init = ICollectionTransformation.init(ic)
        (ic :+ src, init.map(pulses.scanLeft(_)(_ :+ _)))
      },
      List(src -> pulses)
    )
  }

  def prepend[A](pulses: List[A]): ICollectionTransformation[A, List[A]] = {
    val src = Event.source[A]

    ICollectionTransformation[A, List[A]](
      ic => {
        val init = ICollectionTransformation.init(ic)
        (src +: ic, init.map(pulses.scanLeft(_)((acc, n) => n +: acc)))
      },
      List(src -> pulses)
    )
  }

  def updated[A](
      pulses: List[(Int, A)]): ICollectionTransformation[A, List[A]] = {
    val src = Event.source[(Int, A)]

    ICollectionTransformation[A, List[A]](
      ic => {
        val init = ICollectionTransformation.init(ic)
        (ic.updated(src), init.map(pulses.scanLeft(_) {
          case (acc, (idx, el)) => acc.updated(idx, el)
        }))
      },
      List(src -> pulses)
    )

  }

}
