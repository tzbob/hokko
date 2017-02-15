package hokko.collection

import hokko.core.{Event, FRPTestSuite}

class IVectorTest extends FRPTestSuite {
  describe("IVectors") {
    describe("that have deletions") {

      val testRange = 1 to 5
      val ivC       = IVector.constant(testRange.toVector)
      val src       = Event.source[Int]
      val deletes   = src
      val ivDeleted = ivC.remove(deletes)

      it("should delete values") {
        val results =
          mkOccurrencesWithPulses(ivDeleted.toDBehavior.changes())(
            src,
            List(3, 1, 2, 0))
        assert(
          results === List(
            Vector(1, 2, 3, 5),
            Vector(1, 3, 5),
            Vector(1, 3),
            Vector(3)
          ))
      }

      it("should properly adjust indices on updates") {
        val updated = ivC.updated(deletes.map(x => x -> 4)).remove(deletes)
        val results =
          mkOccurrencesWithPulses(updated.toDBehavior.changes)(src, List(1))
        assert(
          results === List(
            Vector(1, 3, 4, 1)
          ))

        val updateAfterDelete = ivDeleted.updated(deletes.map(x => x -> 3))
        val results2 =
          mkOccurrencesWithPulses(updateAfterDelete.toDBehavior.changes)(
            src,
            List(1))
        assert(
          results2 === List(
            Vector(1, 3, 4, 1)
          ))

      }

    }

    describe("that are updated") {

      val testRange     = 1 to 20
      val ivC           = IVector.constant(testRange.toVector)
      val src           = Event.source[Int]
      val evtUpdateHead = src.map(x => x -> 10)
      val ivUpdated10   = ivC.updated(evtUpdateHead)

      it("should always return the correct value at position 10") {
        check { (ints: List[Int]) =>
          val pos10 = ivUpdated10(10)
          val occs  = mkOccurrencesWithPulses(pos10.changes())(src, ints)
          occs == ints
        }
      }

      it("should always return the correct size") {
        check { (ints: List[Int]) =>
          val size = ivUpdated10.size
          val occs = mkOccurrencesWithPulses(size.changes())(src, ints)
          occs == ints.map(_ => 20)
        }
      }

      it("should always return the correct count") {
        check { (ints: List[Int]) =>
          val f: Int => Boolean = x => {
            x == 0 || x % 2 == 0
          }
          val count         = testRange.count(f)
          def plus1(x: Int) = if (f(x)) 1 else 0
          val result = ints
            .scanLeft(count - plus1(testRange(10))) { (acc, i) =>
              acc + plus1(i)
            }
            .tail
          val eventCount = ivUpdated10.count(f)
          val occs       = mkOccurrencesWithPulses(eventCount.changes())(src, ints)
          occs === result
        }
      }

      it("should always return the mapped seq") {
        check { (ints: List[Int]) =>
          val mapped = ivUpdated10.map(_ + 1)
          val occs =
            mkOccurrencesWithPulses(mapped.toDBehavior.changes())(src, ints)
          occs == ints.map { x =>
            testRange.updated(10, x).map(_ + 1)
          }
        }
      }

    }
  }
}
