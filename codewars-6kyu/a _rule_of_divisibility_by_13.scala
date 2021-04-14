import scala.annotation.tailrec

object Thirteen {

  val mods = List(1,10,9,12,3,4)

  def thirt(n: Long): Long = {
    val n2 = n.toString.split("").reverse
    val n3 = n2.map(i=>i.toLong)
    thirtRecursive(n3,n.toLong)
    // your code
  }
  @tailrec
  def thirtRecursive(n:Array[Long], nPrev:Long): Long = {
    var i = 0
    val n3 = n.map(elem => {
      if(i==6) {
        i=0
      }
      val number = mods(i)
      i=i+1
      elem*number
    }).sum
    if(n3 == nPrev ) return nPrev
    thirtRecursive(n3.toString.split("").reverse.map(i=>i.toLong), n3)

  }
}
