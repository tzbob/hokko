package hokko.core

import java.util.concurrent.atomic.AtomicInteger

import cats.effect.IO
import hokko.control.Description
import org.scalatest.AsyncFunSuite

import scala.concurrent.Future

class AsyncTest extends AsyncFunSuite {

  test("Asynchronous IO is executed in two ticks") {
    var counter = 0

    def addCounter[A](ev: Event[A]): Event[A] = ev.map { a =>
      counter += 1
      a
    }

    val src      = Event.source[IO[Long]]
    val executed = addCounter(Async.execute(addCounter(src)))

    val desc = Description
    // when source executes there should only have been 1 tick
      .subscribe(src) { v: IO[Long] =>
        assert(v.unsafeRunSync() === 234)
        assert(counter === 1)
      }
      // when executed executes there were 2 ticks
      .subscribe(executed) { v: Long =>
        assert(v === 234)
        assert(counter === 2)
      }
    val network = desc.compile()

    assert(counter === 0)

    val getTime = IO { 234L }
    val results = network.engine.fire(Seq(src -> getTime))

    Future.sequence(results.futurePropagations).map(_ => assert(counter === 2))
  }

  test("Asynchronous IO is executed asynchronously") {
    val counter = new AtomicInteger(0)

    def addCounter[A](ev: Event[A]): Event[A] = ev.map { a =>
      counter.incrementAndGet()
      a
    }

    val src      = Event.source[Int]
    val sleepSrc = Event.source[IO[Int]]
    val executed = addCounter(Async.execute(addCounter(sleepSrc)))

    val desc = Description
      .subscribe(sleepSrc) { _: IO[Int] =>
        assert(counter.get() === 1)
      }
      .subscribe(addCounter(src)) { v: Int =>
        assert(v === 50)
        assert(counter.get() === 2)
      }
      .subscribe(executed) { v: Int =>
        assert(v === 123)
        assert(counter.get() === 3)
      }
    val network = desc.compile()

    val sleepAndReturn = IO {
      Thread.sleep(1000)
      123
    }

    val sleepResults = network.engine.fire(Seq(sleepSrc -> sleepAndReturn))
    val syncResults  = network.engine.fire(Seq(src      -> 50))

    Future
      .sequence(
        sleepResults.futurePropagations ++ syncResults.futurePropagations)
      .map(_ => assert(counter.get() === 3))
  }
}
