
// LINK : https://www.codewars.com/kata/54bf1c2cd5b56cc47f0007a1

object Dups {
  def duplicateCount(str: String): Int = {
    str.split("").toList.map(x=>x.toLowerCase)
      .groupBy(x=>x).map(x=>x._2.size-1).filter(x=>x>=1).toList.size
  }
}
