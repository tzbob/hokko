package hokko.core

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import cats.syntax.all._
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

class DBehaviorTest extends FunSuite with FRPSuite with Checkers {
  val src = Event.source[Int]
  val intDBehavior: DBehavior[Int] = src
    .fold(0) { (acc, n) =>
      n
    }

  test("Constant DBehaviors return constants and don't have occurences") {
    val const = DBehavior.constant(5)
    val test = const.map { i =>
      i
    }
    val occurrences = mkOccurrences(test.changes) { implicit engine =>
      val currentValues = engine.askCurrentValues()
      assert(currentValues(const.toCBehavior).get === 5)
    }
    assert(occurrences === List.empty)
  }

  test("DBehaviors can be applied") {
    val bPoorMansDouble = intDBehavior.map { (i: Int) => (int: Int) =>
      int + i
    }
    val bApplied = bPoorMansDouble ap intDBehavior
    check { (ints: List[Int]) =>
      val occs = mkOccurrences(bApplied.changes) { implicit engine =>
        fireAll(src, ints)
        val currentValues = engine.askCurrentValues()
        assert(
          currentValues(bApplied.toCBehavior).get === ints.lastOption
            .map(_ * 2)
            .getOrElse(0))
      }
      occs == ints.map(_ * 2)
    }
  }

  test("DBehaviors can be applied but may not misfire") {
    val bPoorMansDouble = intDBehavior.map { (i: Int) => (int: Int) =>
      int + i
    }
    val bApplied = bPoorMansDouble ap intDBehavior
    check { (ints: List[Int]) =>
      val occs = mkOccurrences(bApplied.changes) { implicit engine =>
        fireAll(Event.source[Int], ints)
      }
      occs == List()
    }
  }

  test("DBehaviors can be diffed into IBehaviors") {
    val ib = intDBehavior.toIBehavior(_ - _)(_ + _)
    check { (ints: List[Int]) =>
      val occurrences = mkOccurrencesWithPulses(ib.deltas)(src, ints)
      val expected = (0 +: ints, (0 +: ints).tail).zipped.map { (o, n) =>
        n - o
      }
      occurrences == expected
    }
  }

  test("DBehaviors can be snapshot") {
    val b1      = DBehavior.constant(5)
    val b2      = DBehavior.constant(3)
    val src     = Event.source[Int]
    val snapped = b1.map2(b2)(_ + _).snapshotWith(src: Event[Int])(_ + _)

    check { (ints: List[Int]) =>
      val occs = mkOccurrences(snapped) { implicit engine =>
        ints.foreach { i =>
          engine.fire(List(src -> i))
        }
      }
      occs == ints.map(_ + 5 + 3)
    }
  }

  test("DBehaviors can be delayed") {
    val delayed = DBehavior.delayed(intDBehavior)

    check { (ints: List[Int]) =>
      implicit val engine = Engine.compile(delayed)
      fireAll(src, ints)
      val currentValues = engine.askCurrentValues()
      val allAsserted = ints.lastOption.map { last =>
        assert(currentValues(delayed).get === last)
      }
      true
    }

  }
}
