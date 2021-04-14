import scala.collection.mutable.ArrayBuffer

object Kata {

  def mxdiflg(a1: List[String], a2: List[String]): Int = {

    if(a1.isEmpty || a2.isEmpty) -1
    else{
      val listItems: ArrayBuffer[Int] = ArrayBuffer()
      a1.foreach(elem1 => a2.foreach(elem2 => listItems.append( (elem1.length-elem2.length).abs )))
      listItems.max

      // max( (a1.length - a2.length).abs)
    }
    
    
  }
}
