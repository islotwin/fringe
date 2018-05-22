
object HelloWorld extends App {

  val startN = Set[Node[Int]]()
  val startE = Set[Edge]()
  val graph = Graph[Int](startN, startE)
    .addNode(Node[Int](1, 69))
    .addNode(Node[Int](2, 69))
    .addNode(Node[Int](3, 69))
    .addNode(Node[Int](4, 69))
    .addNode(Node[Int](5, 100))
    .addEdge(Edge(1, 2, 1.0))
    .addEdge(Edge(1, 5, 5.0))
    .addEdge(Edge(5, 3, 5.0))
    .addEdge(Edge(1, 3, 1.0))
    .addEdge(Edge(3, 2, 1.0))
    .addEdge(Edge(3, 4, 1.0))
    .addEdge(Edge(2, 4, 4.0))




  println(graph)

  val childs1 = graph.findChildren(1)
  println(childs1)

  val parent2 = graph.findParents(2)
  println(parent2)

  def h[T](cost: Int, graph: Graph[T]): Double = {
    0
  }
  val fringe = FringeSearch(graph, 1, 4, h[Int])
  val res = fringe.run()
  println(res)
}
