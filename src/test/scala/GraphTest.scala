import org.scalatest.FunSuite;

class GraphTest extends FunSuite {

	test("addNode") {
		val graph = Graph[Int]()
			.addNode(Node[Int](1, 69))
		
		assert(graph.nodes === Set(Node(1,69)))
    }
  
	test("addMultipleNodes") {
		val graph = Graph[Int]()
			.addNode(Node[Int](1, 69))
			.addNode(Node[Int](2,420))
	
		assert(graph.nodes === Set(Node(1,69), Node(2,420)))
    }
  
    test("addNodeWithExistingId") {
		assertThrows[Exception] {
			val graph = Graph[Int]()
			.addNode(Node[Int](1, 69))
			.addNode(Node[Int](1,420))
			}
    }
	
	test("addEdge") {
		val graph = Graph[Int]()
			.addNode(Node[Int](1, 69))
			.addNode(Node[Int](2, 420))
			.addEdge(Edge(1, 2, 6.0))
		
		assert(graph.edges === Set(Edge(1, 2, 6.0)))
	}
	
      test("addMultipleEdges") {
		val graph = Graph[Int]()
			.addNode(Node[Int](1, 69))
			.addNode(Node[Int](2,420))
			.addNode(Node[Int](3,123))
			.addEdge(Edge(1, 2, 6.0))
			.addEdge(Edge(2, 3, 16.0))
		
		assert(graph.edges === Set(Edge(1, 2, 6.0), Edge(2, 3, 16.0)))
	}

  
    test("addExistingEdge") {
		assertThrows[Exception] {
			val graph = Graph[Int]()
			.addNode(Node[Int](1, 69))
			.addNode(Node[Int](1,420))
			.addEdge(Edge(1, 2, 6.0))
			.addEdge(Edge(1, 2, 6.0))
			}
    }
	
	test("addEdgeWithoutNodes") {
		assertThrows[Exception] {
			val graph = Graph[Int]()
				.addNode(Node[Int](1, 69))
				.addEdge(Edge(1, 2, 6.0))
			}
    }
	
	test("removeEdge") {
		val graph = Graph[Int]()
			.addNode(Node[Int](1, 69))
			.addNode(Node[Int](2, 420))
			.addEdge(Edge(1, 2, 6.0))
			.removeEdge(1,2)
		
		assert(graph.edges === Set())
	}
	
	test("removeEdgeThatDoesntExist") {
		assertThrows[Exception] {
			val graph = Graph[Int]()
				.addNode(Node[Int](1, 69))
				.addNode(Node[Int](2, 420))
				.removeEdge(1,2)
		}
	}
	
	test("removeNode") {
		val graph = Graph[Int]()
			.addNode(Node[Int](1, 69))
			.addNode(Node[Int](2, 420))
			.removeNode(2)
		
		assert(graph.nodes === Set(Node(1,69)))
	}
	
	test("removeNodeWithEgdes") {
		val graph = Graph[Int]()
			.addNode(Node[Int](1, 69))
			.addNode(Node[Int](2, 420))
			.addNode(Node[Int](3, 420))
			.addEdge(Edge(1, 2, 5.0))
			.addEdge(Edge(3, 2, 15.0))
			.removeNode(3)
		
		assert(graph.nodes === Set(Node(1,69), Node(2, 420)))
		assert(graph.edges === Set(Edge(1, 2, 5.0)))
	}
	
	test("removeNodeThatDoesntExist") {
		assertThrows[Exception] {
			val graph = Graph[Int]()
				.addNode(Node[Int](1, 69))
				.addNode(Node[Int](2, 420))
				.removeNode(3)
		}
	}
	
	
	test("findChildren") {
		val children = Graph[Int]()
			.addNode(Node[Int](1, 69))
			.addNode(Node[Int](2, 420))
			.addNode(Node[Int](3, 420))
			.addEdge(Edge(1, 2, 5.0))
			.addEdge(Edge(2, 3, 15.0))
			.findChildren(2)

		
		assert(children._1 === Set(Node(3,420)))
		assert(children._2 === Set(Edge(2,3,15.0)))
	}
	
	
  }
