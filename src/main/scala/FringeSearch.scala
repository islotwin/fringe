case class FringeSearch[T](graph: Graph[T], start: Int, goal: Int, h: (Int, Graph[T]) => Double) {

  /**
    * run the algorithm
    *
    * @return path of nodes' indices from start to goal
    */
  def run(): List[Int] = {
    if (start != goal) {
      val result = aggregateWhile((Map[ Int, (Double, Option[Int])]((start, (0, None)), goal -> null), List(start), Double.PositiveInfinity), h(start, graph))
      // cache = Map(node_id, (cost_to_node, parent))
      val cache = result._1._1
      if (isSolved(cache)) {
        val path = findPath(cache, start, goal, List((goal, cache(goal))))._4

        path.map(n => n._1)
      }
      else {
        List()
      }
    }
    else {
      List()
    }
  }

  /**
    * recursive function instead of while in pseudocode to avoid vars
    * @param tuple (cache, fringe, fmin) current temporary values in each call
    *              cache keeps the best parents of nodes already visited
    *              fringe is a list of nodes to visit in this call
    *              fMin the minimal cost of path that exceeds fLimit
    * @param fLimit cost limit to which we search through children nodes
    * @return tuple after current call
    */
  def aggregateWhile(tuple: (Map[ Int, (Double, Option[Int])], List[Int], Double), fLimit: Double)
  : (( Map[ Int, (Double, Option[Int])], List[Int], Double ), Double) = {
    // r: accumulator(cache, fringe, fmin), n: current_id
    val result = tuple._2.foldLeft(tuple._1, tuple._2, Double.PositiveInfinity)((r: (Map[Int, (Double, Option[Int])], List[Int], Double), n: Int) => {
        val currentCache = r._1
        val currentFringe = r._2
        val currentFMin = r._3
        val visited: (Double, Option[Int]) = currentCache(n)
        val f: Double = visited._1 + h(n, graph)

        // if current cost exceeds fLimit, ignore current path
        if (f > fLimit) {
          (currentCache, currentFringe, if (f > currentFMin) currentFMin else f)
        }
        else if (n != goal) {
          // expand children form current node
          val successors = graph.findChildren(n)

          // get children from right to later get nodes in the same order
          val currentResult = successors._1.foldRight(r._2, r._1)((s: Node[T], rr: (List[Int], Map[Int, (Double, Option[Int])])) => {
            val succCache = rr._2
            val succFringe = rr._1

            // calculate cost of getting to current node
            val g = visited._1 + successors._2.find(e => e.toId == s.id).get.cost
            // if current path costs less than previously found, update cache
            if (!(currentCache.get(s.id).nonEmpty && currentCache(s.id) != null && g > currentCache(s.id)._1)) {
              ((succFringe.diff(List(s.id)).takeWhile(_ != n) :+ s.id) ::: succFringe.diff(List(s.id)).dropWhile(_ != n), succCache + (s.id -> (g, Some(n))))
            }
            else {
              rr
            }
          })
          (currentResult._2, currentResult._1.diff(List(n)), if (f > currentFMin) currentFMin else f)
        }
        else {
          r
        }
      })

    val fMin = result._3

    // if path is not found and fringe is not empty, continue search
    if(!(isSolved(result._1) || result._2.isEmpty)) {
      aggregateWhile(result, if(fMin != Double.PositiveInfinity) fMin else fLimit)
    }
    else {
      (result, fLimit)
    }
  }

  /**
    * check whether goal has parent in cache, if so - solution found
    * @param cache
    * @return path from goal to start is found
    */
  def isSolved(cache : Map[ Int, (Double, Option[Int])]) : Boolean = {
    cache(goal) != null
  }


  /**
    * recursive function to find path from cache, go from goal node upwards (up to start node)
    * @param cache
    * @param start start node id
    * @param goal goal node id
    * @param path
    * @return cache, start, goal are additional variables, returns path from start to goal containing tuples same as in cache (node_id, (cost, parent))
    */
  def findPath(cache:  Map[ Int, (Double, Option[Int])], start: Int, goal: Int, path: List[ (Int, (Double, Option[Int]))]) : (Map[ Int, (Double, Option[Int])],
    Int, Int, List[ (Int, (Double, Option[Int]))]) = {
    if(path.head._1 == start) {
      (cache, start, goal, path)
    }
    else {
      val current = cache(path.head._1)
      val parent = current._2
      if(parent.nonEmpty && parent.get != null){
        val p = cache(parent.get)

        // add parent node to start of the path
        findPath(cache, start, goal, (parent.get, p) +: path)
      }
      else {
        (cache, start, goal, path)
      }
    }
  }

}
