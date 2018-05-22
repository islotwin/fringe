case class FringeSearch[T](graph: Graph[T], start: Int, goal: Int, h: (Int, Graph[T]) => Double) {

  def run(): List[Int] = {
    if (start != goal) {
      val result = findPath((Map[ Int, (Double, Option[Int])]((start, (0, None)), goal -> null), List(start), Double.PositiveInfinity), h(start, graph))
      val cache = result._1._1
      if (isSolved(cache)) {
        println(cache)
        cache.toList.map(c => c._1)
      }
      else {
        List()
      }
    }
    else {
      List()
    }
  }

  // (cache, fringe, fmin), fLimit
  def findPath(tuple: (Map[ Int, (Double, Option[Int])], List[Int], Double), fLimit: Double)
  : (( Map[ Int, (Double, Option[Int])], List[Int], Double ), Double) = {
    val result = tuple._2
      .foldLeft(tuple._1, tuple._2, Double.PositiveInfinity)((r: (Map[Int, (Double, Option[Int])], List[Int], Double), n: Int) => {
        val currentCache = r._1
        val currentFringe = r._2
        val currentFMin = r._3
        val visited: (Double, Option[Int]) = currentCache(n)
        val f: Double = visited._1 + h(n, graph)

        if (f > fLimit) {
          (currentCache, currentFringe, if (f > currentFMin) currentFMin else f)
        }
        else if (n != goal) {
          val successors = graph.findChildren(n) // iterate from right
          val currentResult = successors._1.foldLeft(r._1, r._2)((rr: (Map[Int, (Double, Option[Int])], List[Int]), s: Node[T]) => {
            val succCache = rr._1
            val succFringe = rr._2
            val g = visited._1 + successors._2.find(e => e.toId == s.id).get.cost
            if (!(currentCache.get(s.id).nonEmpty && currentCache(s.id) != null && g > currentCache(s.id)._1)) {
              (succCache + (s.id -> (g, Some(n))), (succFringe.diff(List(s.id)).takeWhile(_ != n) :+ s.id) ::: succFringe.diff(List(s.id)).dropWhile(_ != n))
            }
            else {
              rr
            }
          })
          (currentResult._1, currentResult._2.diff(List(n)), if (f > currentFMin) currentFMin else f)
        }
        else {
          r
        }
      })

    val fMin = result._3

    if(!(isSolved(result._1) || result._2.isEmpty)) {
      findPath(result, if(fMin != Double.PositiveInfinity) fMin else fLimit)
    }
    else {
      (result, fLimit)
    }
  }

  def isSolved(cache : Map[ Int, (Double, Option[Int])]) : Boolean = {
    cache(goal) != null
  }

}