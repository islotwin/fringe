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

  test("noPath") {

    val graph = Graph[Int]()
      .addNode(Node[Int](1, 69))
      .addNode(Node[Int](2, 69))
      .addNode(Node[Int](3, 69))
      .addEdge(Edge(1, 2, 1.0))

    def h[T](cost: Int, graph: Graph[T]): Double = { 0 }
    val fringe = FringeSearch(graph, 1, 3, h[Int])
    val res = fringe.run()

    assert(res.isEmpty)
  }

  test("sameNode") {
    val graph = Graph[Int]()
      .addNode(Node[Int](1, 69))
      .addNode(Node[Int](2, 69))
      .addEdge(Edge(1, 2, 4.0))

    def h[T](cost: Int, graph: Graph[T]): Double = { 0 }
    val fringe = FringeSearch(graph, 1, 1, h[Int])
    val res = fringe.run()

    println(res)
    assert(res.isEmpty)
  }

  test("heuristic") {
    val graph = Graph[Double]()
      .addNode(Node[Double](1, 69.0))
      .addNode(Node[Double](3, 100.0))
      .addNode(Node[Double](2, 10.0))
      .addNode(Node[Double](4, 10.0))
      .addEdge(Edge(1, 2, 20.0))
      .addEdge(Edge(2, 4, 20.0))
      .addEdge(Edge(1, 3, 1.0))
      .addEdge(Edge(3, 4, 1.0))

    def h[T](node: Int, graph: Graph[T]): Double = { graph.nodes.find(n => n.id == node).get.value.asInstanceOf[Double] }
    val fringe = FringeSearch(graph, 1, 4, h[Double])
    val res = fringe.run()

    assert(res === List(1, 2, 4))
  }

}
