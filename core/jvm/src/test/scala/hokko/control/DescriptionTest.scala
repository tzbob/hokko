package hokko.control

import hokko.core._
import org.scalatest.FunSuite

class DescriptionTest extends FunSuite {

  test("Descriptions can be used to read behaviors") {
    var int    = 0
    val intSrc = CBehavior.source(0)
    intSrc.changeSource(Some(int))

    val network = Description.read(intSrc).compile()

    assert(network.now() === 0)
    int = 20
    assert(network.now() === 20)
  }

  test("Descriptions can be used to subscribe on evens") {
    val src = Event.source[Int]

    val network = Description
      .subscribe(src) { e =>
        assert(e === 10)
        ()
      }
      .compile()

    network.engine.fire(Seq(src -> 10))
  }

}
