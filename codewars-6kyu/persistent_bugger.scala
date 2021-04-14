object Multiplication {

  def persistence(n: Int): Int = {
    persistence2(n,0)
  }
  def persistence2(n: Int, count:Int): Int = {
    if(n.toString.length<=1)count
    else persistence2(n.toString.split("").map(elem=>elem.toInt).product,count+1)
  }
}
