package hokko.core

import hokko.control.Description
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

class CBehaviorTest extends FunSuite with FRPSuite with Checkers {
  test("Constant Behaviors return constants and don't have occurences") {
    val const         = CBehavior.constant(5)
    val engine        = Engine.compile(const)
    val currentValues = engine.askCurrentValues()
    assert(currentValues(const).get === 5)
  }

  test("Behaviors can be created by polling") {
    var i      = 0
    val beh    = CBehavior.fromPoll(() => i)
    val engine = Engine.compile(beh)
    check { (int: Int) =>
      i = int
      val values = engine.askCurrentValues()
      values(beh).get == int
    }
  }

  test(
    "Behavior sources can be set to produce different values and unset to " +
      "produce defaults") {
    val intSrc = CBehavior.source(0)
    intSrc.changeSource(Some(20))
    val network = Description.read(intSrc).compile()
    assert(network.now() === 20)
    intSrc.unSet()
    assert(network.now() === 0)
  }

  test("Behaviors can be applied") {
    var param = 0
    var f     = (i: Int) => i + 0

    val bParam = CBehavior.fromPoll(() => param)
    val bFun   = CBehavior.fromPoll(() => f)

    val bApplied = bFun ap bParam
    val engine   = Engine.compile(bApplied)
    check { (int: Int) =>
      param = int
      f = (i: Int) => i + int
      val currentValues = engine.askCurrentValues()
      currentValues(bApplied).get == int * 2
    }
  }

  test("Behaviors can be snapshot by events") {
    var param   = 0
    val bParam  = CBehavior.fromPoll(() => param)
    val src     = Event.source[Int]
    val snapped = bParam.snapshotWith(src: Event[Int])(_ + _)

    val test = CBehavior.source(0)
    test.map2(bParam)(_ + _)

    check { (ints: List[Int]) =>
      val occs = mkOccurrences(snapped) { implicit engine =>
        ints.foreach { i =>
          param = i
          engine.fire(List(src -> i))
        }
      }
      occs == ints.map(_ * 2)
    }
  }

  test("Behaviors can be snapshot by discrete behaviors") {
    var param   = 0
    val bParam  = CBehavior.fromPoll(() => param)
    val src     = Event.source[Int]
    val b       = src.fold(0)((_, n) => n).toDBehavior
    val snapped = bParam.snapshotWith(b)(_ + _)

    check { (ints: List[Int]) =>
      assert(snapped.init === 0)
      val occs = mkOccurrences(snapped.changes) { implicit engine =>
        ints.foreach { i =>
          param = i
          engine.fire(List(src -> i))
        }
      }
      occs == ints.map(_ * 2)
    }
  }
}
