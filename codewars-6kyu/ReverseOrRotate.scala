
// LINK : https://www.codewars.com/kata/56b5afb4ed1f6d5fb0000991

import scala.collection.mutable.ArrayBuffer

object RevRot {

  def revRot(strng: String, sz: Int): String = {
    // your code
    if(sz <= 0 || strng.isEmpty || sz > strng.length) ""
    else{
      val newString : ArrayBuffer[String] = ArrayBuffer()
      val substrings = strng.grouped(sz)

      substrings.foreach(str => {
        if(str.length >= sz){
          val number = str.map( digit => digit*digit*digit).sum
          if(number % 2 == 0) {
            newString.append(str.reverse)
          }else{
            newString.append(str.substring(1) + str.substring(0, 1))
          }
        }
      })
      newString.mkString("")
    }
  }
}
