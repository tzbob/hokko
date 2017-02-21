package hokko.collection

import cats.syntax.apply._
import hokko.collection.ICollection.ICollection
import hokko.control.Description
import hokko.core._

import scala.language.existentials

object ICollectionTransformation {
  def init[El, Coll](ic: ICollection[El, Coll]): Description[Coll] =
    Description.read(ic.toCBehavior)
}

case class ICollectionTransformation[El, Coll](
    f: ICollection[El, Coll] => (ICollection[El, Coll],
                                 Description[List[Coll]]),
    pulses: List[(EventSource[A], List[A]) forSome { type A }]
) {
  def chain(other: ICollectionTransformation[El, Coll])
    : ICollectionTransformation[El, Coll] = {
    ICollectionTransformation(ic => {
      val (ic1, results1) = this.f(ic)
      val (ic2, results2) = other.f(ic1)
      (ic2, results1.map2(results2)(_ ++ _.tail))
    }, pulses ++ other.pulses)
  }
}
