object LongestConsec {

  def longestConsec(strarr: Array[String], k: Int): String = {
    if(strarr.length==0 || k>strarr.length || k<=0) return ""
    val slided = strarr.sliding(k).toList.map(u=>u.mkString(""))
    val sizes = slided.map(u=>u.length).max
    slided.filter(u=>u.length==sizes).head

  }
}
