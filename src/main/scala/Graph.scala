/**
 * Class to represent graph. On this graph will run FringeSearch 
 * to find the shortest path to given node. 
 * Contains basic methods to create and maintain graph.
 * @constructor Construct graph with set of nodes and edges
 * @param newNodes set of nodes
 * @param newEdges set of edges
 */
case class Graph[T](newNodes: Set[Node[T]] = null, newEdges: Set[Edge] = null) {
  /**
   * Set of nodes in graph
   */
  val nodes : Set[Node[T]] = newNodes
  
  /**
   * Set of edges in graph
   */
  val edges : Set[Edge] = newEdges

  /**
   * Add edge to graph. 
   * @param edge Edge to be added
   * @throws Exception if given edge connects nodes that doesn't exist,
   * 				 with same source and target or if edge already exists
   * @return Graph with added edge
   */
  def addEdge(edge : Edge) : Graph[T] = {

    if (!(nodes.exists(x => x.id == edge.fromId) && nodes.exists(x => x.id == edge.toId))) {
      throw new Exception("Given node doesn't exist");
    }
    else if(edge.fromId == edge.toId) {
      throw new Exception("Can't add edge with same source and target");
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

   /**
   * Add node to graph. 
   * @param node Node to be added
   * @throws Exception if given node already exists
   * @return Graph with added node
   */
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
  
   /**
   * Remove given edge from graph
   * @param edge Edge to be removed
   * @throws Exception if given edge doesn't exists
   * @return Graph with removed edge
   */
  def removeEdge(edge : Edge) : Graph[T] = {
    if(!edges.exists(x=> x.fromId == edge.fromId && x.toId == edge.toId)){
      throw new Exception("Given edge doesn't exist");
    }

    else {
      Graph(nodes, edges - edge)
    }
  }
   /**
   * Remove edge from graph with given id's of nodes
   * @param idFromDel Edge to be removed
   * @param idToDel Edge to be removed
   * @throws Exception if given edge doesn't exists
   * @return Graph with removed edge
   */
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

   /**
   * Remove node with given id.
   * @param idToDelete Id of node which will be removed
   * @throws Exception if given id doesn't exists
   * @return Graph with removed node
   */
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
   /**
   * Finds node's with given id children 
   * @param nodeId id of parent node
   * @throws Exception if node with given of doesn't exists
   */
  def findChildren(nodeId : Int) : (Set[Node[T]], Set[Edge]) = {
    val children = edges.filter(e => e.fromId == nodeId)
    if (children == null){
      throw new Exception("Given node doesn't exists");
    }
    else {
      (nodes.filter(n => children.exists(_.toId == n.id)), children);
    }
  }
}

case class Node[T](id : Int, value : T);
case class Edge(fromId: Int, toId : Int, cost : Double);
