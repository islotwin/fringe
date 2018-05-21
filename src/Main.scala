

class Main {

}

object HelloWorld{
  
  def main(args: Array[String]): Unit = {
  
  case class Graph[T](newNodes: Set[Node[T]] = null, newEdges: Set[Edge] = null) {
    val nodes : Set[Node[T]] = newNodes;
    val edges : Set[Edge] = newEdges;
    def addEdge(edge : Edge) : Graph[T] = {
      new Graph(nodes, edges + edge);
    }
    def addNode(node : Node[T]) : Graph[T] = {
      new Graph(nodes + node, edges);
    }
    def removeEdge(edge : Edge) : Graph[T] = {
      new Graph(nodes, edges - edge);
    }
    def removeNode(idToDelete : Int) : Graph[T] = {
      new Graph(nodes.filter(_.id != idToDelete), edges.filter(x => !(x.fromId == idToDelete || x.toId == idToDelete)));
    }
    def removeEdge(idFromDel : Int, idToDel : Int) : Graph[T] = {
      new Graph(nodes, edges.filter(x => !(x.fromId == idFromDel && x.toId == idToDel)));
    }
    
    def findChilds(nodeId : Int) : Set[Node[T]] = {
      nodes.filter(x=> edges.exists(_.toId == x.id));
    }
    
    def findParents(nodeId : Int) : Set[Node[T]] = {
      nodes.filter(x=> edges.exists(_.fromId == x.id));
    }
  }
  
     
  case class Node[T](id : Int, value : T);
  case class Edge(fromId: Int, toId : Int, cost : Double);
  
  
  val startN = Set[Node[Int]]();
  val startE = Set[Edge]();
  val graph = new Graph[Int](startN, startE)
              .addNode(new Node[Int](1, 69))
              .addNode(new Node[Int](2, 69))
              .addNode(new Node[Int](3, 69))
              .addNode(new Node[Int](4, 69))
              .addEdge(new Edge(1,2,1.0))
              .addEdge(new Edge(1,3,1.0))
              .addEdge(new Edge(3,2,1.0))
              .addEdge(new Edge(4,3,1.0));

  println(graph);
  
  val childs1 = graph.findChilds(1);
  println(childs1);
  
  val parent2 = graph.findParents(2);
  println(parent2);
  
  }
  
}