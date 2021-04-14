// LINK : https://www.codewars.com/kata/550f22f4d758534c1100025a


import scala.collection.mutable.ArrayBuffer

object DirReduction {

  def dirReduc(arr: Array[String]): Array[String] = {
    val buf = collection.mutable.ArrayBuffer(arr: _*)
    var i = 0
    while(  i < buf.size-1){
      if( (buf(i) == "NORTH" && buf(i+1) == "SOUTH") || (buf(i) == "SOUTH" && buf(i+1) == "NORTH") ||
        (buf(i) == "EAST" && buf(i+1) == "WEST") || (buf(i) == "WEST" && buf(i+1) == "EAST") ){
        buf.remove(i)
        buf.remove(i)
        i = 0
      }else i+=1
    }


    buf.toArray
  }
}
