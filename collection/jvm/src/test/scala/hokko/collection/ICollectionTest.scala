package hokko.collection

import hokko.collection.ICollection.ICollection
import hokko.collection.ICollection.implicits._

class ICollectionTest extends SeqIBehaviorTests {
  describe("ICollections") {

    implicit val arb = arbitraryUpdated[Int]

    describe("that are traversable") {
      it("can be concatenated") {
        check { (initial: List[Int], pulses: List[List[Int]]) =>
          val constantInt = ICollection.constant(initial)
          val (changes, expected) =
            mkOccurrencesWithTransformation(constantInt, concatenate(pulses))

          changes === expected.tail
        }
      }

      describe("that are a sequence") {
        it("can be appended") {
          check { (initial: List[Int], pulses: List[Int]) =>
            val constantInt = ICollection.constant(initial)
            val (changes, expected) =
              mkOccurrencesWithTransformation(constantInt, append(pulses))

            changes === expected.tail
          }
        }

        it("can be prepended") {
          check { (initial: List[Int], pulses: List[Int]) =>
            val constantInt = ICollection.constant(initial)
            val (changes, expected) =
              mkOccurrencesWithTransformation(constantInt, prepend(pulses))

            changes === expected.tail
          }
        }

        it("can be updated") {
          check { (input: (List[Int], List[(Int, Int)])) =>
            val (initial, pulses) = input

            val constantInt = ICollection.constant(initial)
            val (changes, expected) =
              mkOccurrencesWithTransformation(constantInt, updated(pulses))

            changes === expected.tail
          }
        }

        it("can be folded") {
          check {
            (input: (List[Int], List[(Int, Int)]), concPulses: List[List[Int]]) =>
              val (initial, pulses) = input

              val constantInt = ICollection.constant(initial)

              val transformation =
                updated(pulses)
                  .chain(append(initial))
                  .chain(prepend(initial))
                  .chain(concatenate(concPulses))

              val (changes, transformationResults) =
                mkOccurrencesWithTransformationF(constantInt, transformation) {
                  (ic: ICollection[Int, List[Int]]) =>
                    ic.foldUndo(0)(_ + _)(_ - _).changes
                }

              val expectedResults = transformationResults.map(_.sum)

              changes === expectedResults.tail
          }
        }
      }
    }
  }
}