case class Graph[T](newNodes: Set[Node[T]] = null, newEdges: Set[Edge] = null) {
  val nodes : Set[Node[T]] = newNodes
  val edges : Set[Edge] = newEdges

  def addEdge(edge : Edge) : Graph[T] = {

    if (!(nodes.exists(x => x.id == edge.fromId) && nodes.exists(x => x.id == edge.toId))) {
      throw new Exception("Given node doesn't exist");
    }
    else if(edges == null) {
        Graph(nodes, Set[Edge]() + edge)
    }
    else if(edges.exists(x=> x.fromId == edge.fromId && x.toId == edge.toId)){
      throw new Exception("Given edge already exists");
    }
    else{
      Graph(nodes, edges + edge)
    }
  }

  def addNode(node : Node[T]) : Graph[T] = {
    if(nodes == null)
      Graph(Set[Node[T]]() + node,  edges)
    else if (nodes.exists(_.id == node.id)){
      throw new Exception("Given node already exists");
    }
    else {
      Graph(nodes + node, edges)
    }
  }

  def removeEdge(edge : Edge) : Graph[T] = {
    if(!edges.exists(x=> x.fromId == edge.fromId && x.toId == edge.toId)){
      throw new Exception("Given edge doesn't exist");
    }
    else {
      Graph(nodes, edges - edge)
    }
  }

  def removeNode(idToDelete : Int) : Graph[T] = {
    if(nodes == null) {
      throw new Exception("Any node exists");
    }
    else if(!nodes.exists(x=> x.id == idToDelete)){
      throw new Exception("Given node doesn't exists");
    }
    else{
      if(edges == null) {
        Graph(nodes.filter(_.id != idToDelete), null)
      }
      else{
        Graph(nodes.filter(_.id != idToDelete), edges.filter(x => !(x.fromId == idToDelete || x.toId == idToDelete)))
      }
    }
  }

  def removeEdge(idFromDel : Int, idToDel : Int) : Graph[T] = {
    if(edges == null) { 
      throw new Exception("Any edge exists");
    }
    else if(!edges.exists(x=> x.fromId == idFromDel && x.toId == idToDel)) {
      throw new Exception("Given edge doesn't exist");
    }
    else {
      Graph(nodes, edges.filter(x => !(x.fromId == idFromDel && x.toId == idToDel)));
    }
  }

  def findChildren(nodeId : Int) : (Set[Node[T]], Set[Edge]) = {
    val children = edges.filter(e => e.fromId == nodeId)
    (nodes.filter(n => children.exists(_.toId == n.id)), children);
  }
}

case class Node[T](id : Int, value : T);
case class Edge(fromId: Int, toId : Int, cost : Double);
