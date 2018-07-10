package hokko.core

import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.scalatest.mockito.MockitoSugar
import org.mockito.Mockito._

class EngineTest extends FunSpec with Matchers {

  def N(v: Int, deps: List[Node[_]] = Nil) =
    new Pull[Any] {
      val dependencies: List[Node[_]]             = deps
      def thunk(context: TickContext): Thunk[Any] = Thunk.eager(null)
    }

  lazy val n1         = N(1)
  lazy val n2         = N(2)
  lazy val simpleTree = N(3, List(n1, n2))
  lazy val simpleDescendants: Node[_] => Set[Node[_]] = { n =>
    if (n == simpleTree) Set(n1, n2)
    else Set.empty
  }

  lazy val n1a = N(4)
  lazy val n2a = N(5, List(n1a))

  describe("An Engine") {
    describe("sorting all nodes") {
      it("should return the singleton list for a leaf") {
        val bfsList = Engine.sortedNodes(List(n1))
        assert(bfsList === List(n1))
      }

      it("should return an ordered list for a tree") {
        val bfsList = Engine.sortedNodes(List(simpleTree))
        val sti     = bfsList.indexOf(simpleTree)
        assert(sti > bfsList.indexOf(n1))
        assert(sti > bfsList.indexOf(n2))
      }

      it("should handle multiple trees") {
        val bfsList   = Engine.sortedNodes(List(simpleTree, n2a))
        val srcNodes  = List(n1, n2, n1a)
        val exitNodes = List(simpleTree, n2a)

        for {
          exit <- exitNodes
          src  <- srcNodes
        } assert(bfsList.indexOf(exit) > bfsList.indexOf(src))
      }
    }
  }
}
