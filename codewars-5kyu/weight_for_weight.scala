object WeightSort {

  def orderWeight(strng: String): String = {
    // your code
    val strng2 = strng.replace("  "," ").split(" ")
      .sortWith(sortBy)

    strng2.mkString(" ")
  }
  def sortBy(s1: String, s2: String) = {
    if(s1.split("").map(u=>u.toInt).sum == s2.split("").map(u=>u.toInt).sum)
      s1 < s2
    else s1.split("").map(u=>u.toInt).sum < s2.split("").map(u=>u.toInt).sum
  }
}
