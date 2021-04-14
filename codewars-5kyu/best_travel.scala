object BestTravel {

  def chooseBestSum(t: Int, k: Int, ls: List[Int]): Int = {
    val per = ls.combinations(k).map(u=> u.sum).filter(u=>u<=t)
    if (per.nonEmpty) per.max
    else -1
  }
}
