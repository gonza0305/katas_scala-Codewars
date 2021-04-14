object Dups {
  def duplicateCount(str: String): Int = {
    str.split("").toList.map(x=>x.toLowerCase)
      .groupBy(x=>x).map(x=>x._2.size-1).filter(x=>x>=1).toList.size
  }
}
