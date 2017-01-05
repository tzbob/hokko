package hokko.collection

import cats.{Eval, Later}
import hokko.collection.ICollection.ICollection
import hokko.collection.ICollection.implicits._
import hokko.core._

import scala.collection.mutable.ListBuffer

trait TraversableIBehaviorTests extends FRPTestSuite {

  def mkOccurrencesWithTransformation[A, Repr](
      ic: ICollection[A, Repr],
      transformation: ICollectionTransformation[A, Repr])
    : (List[Repr], List[Repr]) = {
    mkOccurrencesWithTransformationF(ic, transformation)(_.changes)
  }

  def mkOccurrencesWithTransformationF[A, Repr, Result](
      ic: ICollection[A, Repr],
      transformation: ICollectionTransformation[A, Repr])(
      f: ICollection[A, Repr] => Event[Result]
  ): (List[Result], List[Repr]) = {

    val (target, transDesc) = transformation.f(ic)
    val event               = f(target)
    val changesBuffer       = ListBuffer.empty[Result]
    val description = transDesc.subscribe(event) { result =>
      changesBuffer += result
      ()
    }

    val network = description.compile()

    transformation.pulses.foreach {
      case (src, pulses) =>
        fireAll(src, pulses)(network.engine)
    }
    val expectedResults = network.now()

    network.cancelAllSubscriptions(event)

    val changes = changesBuffer.toList

    changes -> expectedResults
  }

  /**
    * @param pulses
    * @tparam A
    *
    * @return A collection transformation that defines the
    *         concatentation on incremental lists with occurrences defined by
    *         pulses.
    */
  def concatenate[A](
      pulses: List[List[A]]): ICollectionTransformation[A, List[A]] = {
    val src = Event.source[List[A]]

    ICollectionTransformation[A, List[A]](ic => {
      val init = ICollectionTransformation.init(ic)
      (ic ++ src.toEvent, init.map(pulses.scanLeft(_)(_ ++ _)))
    }, List(src -> pulses))
  }
}
