package hokko.collection

import hokko.core.{ Engine, Event, FRPTestSuite }
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._

class MapBehaviorTest extends FRPTestSuite {
  describe("MapBehavior") {
    val src = Event.source[(Int, String)]
    val emptyMap = MapBehavior.empty[Int, String]
    val addedMaps = emptyMap + src

    describe("that expands from add events") {
      it("should stay the same when no events occur") {
        val engine = Engine.compile(Seq.empty, Seq(addedMaps))
        assert(engine.askCurrentValues()(addedMaps).get === Map())
      }

      it("should build up all occurrences") {
        check { (ints: List[(Int, String)]) =>
          val engine = Engine.compile(Seq.empty, Seq(addedMaps))
          fireAll(src, ints)(engine)
          engine.askCurrentValues()(addedMaps).get === ints.toMap
        }
      }
    }

    describe("that shrinks from delete events") {
      it("even out when adding and removing elements") {
        check { (ints: List[(Int, String)]) =>
          val ints = List((1, "hello"), (2, "hello"))
          val neutralMaps = addedMaps - src.map(_._1)
          val engine = Engine.compile(Seq.empty, Seq(neutralMaps))
          fireAll(src, ints)(engine)
          engine.askCurrentValues()(neutralMaps).get === Map()
        }
      }
    }

    val default = List(1 -> "hello").toMap
    val filledMap = MapBehavior.fromMap(default)

    describe("that is mapped incrementally") {
      val addedMaps = filledMap + src
      val mappedMap = addedMaps.incrementalMapValues(_ + "mapped")

      it("should map all values accordingly") {
        check { (ints: List[(Int, String)]) =>
          val engine = Engine.compile(Seq.empty, Seq(mappedMap))
          fireAll(src, ints)(engine)
          engine.askCurrentValues()(mappedMap).get === (default ++ ints).mapValues(_ + "mapped")
        }
      }
    }
  }
}
