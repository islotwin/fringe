import org.scalatest.FunSuite

class FringeSearchTest extends FunSuite {

  test("fringeSearch") {

    val graph = Graph[Int]()
      .addNode(Node[Int](1, 69))
      .addNode(Node[Int](2, 69))
      .addNode(Node[Int](3, 69))
      .addNode(Node[Int](4, 69))
      .addNode(Node[Int](5, 100))
      .addEdge(Edge(1, 2, 1.0))
      .addEdge(Edge(5, 3, 5.0))
      .addEdge(Edge(1, 3, 1.0))
      .addEdge(Edge(3, 2, 1.0))
      .addEdge(Edge(3, 4, 1.0))
      .addEdge(Edge(2, 4, 4.0))

    def h[T](cost: Int, graph: Graph[T]): Double = { 0 }
    val fringe = FringeSearch(graph, 1, 4, h[Int])
    val res = fringe.run()

    assert(res === List(1, 3, 4))


  }
	
	
}
