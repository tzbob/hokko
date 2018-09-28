package hokko.core

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

import scala.language.{existentials, reflectiveCalls}

class EventTest extends FunSuite with FRPSuite with Checkers {
  test("Events can be folded") {
    val src = Event.source[Int]
    val beh = src.fold(0)(_ + _).toDBehavior.toCBehavior
    check { (ints: List[Int]) =>
      val engine = Engine.compile(beh)
      fireAll(src, ints)(engine)
      val currentValues = engine.askCurrentValues()
      currentValues(beh).get == ints.sum
    }
  }

  test("Events can union with other events") {
    val src1 = Event.source[Int]
    val src2 = Event.source[Double]

    val test = src1.map(x => x)

    val union =
      src1.map(_.toString).unionWith(src2.map(_.toString)) {
        (_, _).toString
      }

    check { (ints: List[Int]) =>
      val occurrences = mkOccurrencesWithPulses(union)(src1, ints)
      occurrences == ints.map(_.toString)
    }

    check { (doubles: List[Double]) =>
      val occurrences = mkOccurrencesWithPulses(union)(src2, doubles)
      occurrences == doubles.map(_.toString)
    }

    check { (intDoubles: List[(Int, Double)]) =>
      val occurrences = mkOccurrences(union) { engine =>
        intDoubles.foreach {
          case (i, d) => engine.fire(List(src1 -> i, src2 -> d))
        }
      }
      occurrences == intDoubles.map {
        case (i, d) => (i, d).toString
      }
    }
  }

  test("Events can be discarded completely through collect") {
    val src = Event.source[Int]

    val collected = src.collect(_ => None)

    check { (ints: List[Int]) =>
      val occurrences = mkOccurrencesWithPulses(collected)(src, ints)
      occurrences == List.empty
    }
  }

  test("Events can be doubled through collect") {
    val src       = Event.source[Int]
    val collected = src.collect(i => Some(i * 2))

    check { (ints: List[Int]) =>
      val occurrences = mkOccurrencesWithPulses(collected)(src, ints)
      occurrences == ints.map(_ * 2)
    }
  }

  test("Events can be merged") {
    val src    = Event.source[Int]
    val merged = Event.merge(Seq(src, src, src))

    check { (ints: List[Int]) =>
      val occurrences = mkOccurrencesWithPulses(merged)(src, ints)
      occurrences == ints.map { i =>
        Seq(i, i, i)
      }
    }
  }

  test("BUG: Events should not fire when engine is fired empty") {
    var counter = 0
    val src = Event.source[Int]
    val count: IBehavior[Int, Int] = src.fold(0) { (a, b) =>
      counter += 1
      a + b
    }
    val countCB      = count.toCBehavior
    val countChanges = count.changes

    check { (ints: List[Int]) =>
      counter = 0
      val occs = mkOccurrencesWithDependencies(src)(countCB) {
        implicit engine =>
          engine.fire(Seq.empty)
          fireAll(src, ints)
          engine.fire(Seq.empty)
      }

      val isFoldExecutedOnce = counter == ints.size
      occs == ints && isFoldExecutedOnce
    }

  }
}
