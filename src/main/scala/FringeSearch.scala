case class FringeSearch[T](graph: Graph[T], start: Int, goal: Int, h: (Int, Graph[T]) => Double) {

  var fringe = List(start)
  var cache = Map[ Int, (Double, Option[Int])]()  // (id, (g, parent))
  var fLimit = h(start, graph)

  if(start != goal) {
    cache = cache + (start -> (0, None), goal -> null)
  }

  def run(): List[Int] = {
    while(!(isSolved() || fringe.isEmpty)) {
      var fMin = Double.PositiveInfinity
      fringe.foreach(n => {
        val visited: (Double, Option[Int]) = cache(n)
        val f: Double = visited._1 + h(n, graph)
        if (f > fLimit) {
          fMin = if (f > fMin) fMin else f
          // continue
        }
        else if(n != goal) {
          val successors = graph.findChildren(n) // iterate from right
          successors._1.foreach(s => {
            val g = visited._1 + successors._2.find(e => e.toId == s.id).get.cost
            if (!(cache.get(s.id).nonEmpty && cache(s.id) != null && g > cache(s.id)._1)) {
              if (fringe.exists(id => id == s.id)) {
                fringe = fringe.diff(List(s.id))
              }
              // add elem after n
              fringe = (fringe.takeWhile(_ != n) :+ s.id) ::: fringe.dropWhile(_ != n)
              cache = cache + (s.id -> (g, Some(n)))
            }
          })
          fringe = fringe.diff(List(n))
        }
      })
      if(fMin != Double.PositiveInfinity) {
        fLimit = fMin
      }
    }
    if(isSolved()) {
      println(cache)
      cache.toList.map(c => c._1)
    }
    else {
      List()
    }
  }

  def isSolved() : Boolean = {
    cache(goal) != null
  }

}