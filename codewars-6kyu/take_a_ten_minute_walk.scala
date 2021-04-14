object Solution {
  def isValidWalk(walk: Seq[Char]): Boolean = {
    if(walk.length == 10) {
      val aux = walk.groupBy(u => u).map(u => (u._1, u._2.length))
      val n = aux.filter(u => u._1 == 'n').values
      val s = aux.filter(u => u._1 == 's').values
      val e = aux.filter(u => u._1 == 'e').values
      val w = aux.filter(u => u._1 == 'w').values

      if(n.nonEmpty && s.nonEmpty){
        if(n.head != s.head) return false
      }
      if(e.nonEmpty && w.nonEmpty){
        if(e.head != w.head) return false
      }
      true
    } else  false
    }

  }
