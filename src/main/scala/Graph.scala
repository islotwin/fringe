case class Graph[T](newNodes: Set[Node[T]] = null, newEdges: Set[Edge] = null) {
  val nodes : Set[Node[T]] = newNodes
  val edges : Set[Edge] = newEdges

  def addEdge(edge : Edge) : Graph[T] = {
    Graph(nodes, edges + edge)
  }

  def addNode(node : Node[T]) : Graph[T] = {
    Graph(nodes + node, edges)
  }

  def removeEdge(edge : Edge) : Graph[T] = {
    Graph(nodes, edges - edge)
  }

  def removeNode(idToDelete : Int) : Graph[T] = {
    Graph(nodes.filter(_.id != idToDelete), edges.filter(x => !(x.fromId == idToDelete || x.toId == idToDelete)));
  }

  def removeEdge(idFromDel : Int, idToDel : Int) : Graph[T] = {
    Graph(nodes, edges.filter(x => !(x.fromId == idFromDel && x.toId == idToDel)));
  }

  def findChildren(nodeId : Int) : (Set[Node[T]], Set[Edge]) = {
    val children = edges.filter(e => e.fromId == nodeId)
    (nodes.filter(n => children.exists(_.toId == n.id)), children);
  }

  def findParents(nodeId : Int) : Set[Node[T]] = {
    val parents = edges.filter(e => e.toId == nodeId)
    nodes.filter(x => edges.exists(_.toId == x.id));
  }
}

case class Node[T](id : Int, value : T);
case class Edge(fromId: Int, toId : Int, cost : Double);
