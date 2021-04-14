
// LINK :  https://www.codewars.com/kata/54b42f9314d9229fd6000d9c

import scala.collection.mutable.ArrayBuffer

object Solution {

  def duplicateEncode(word: String) = {
    val aux: ArrayBuffer[String] = new ArrayBuffer[String]()
    word.foreach(char => {
      if( word.count(_.toString.equalsIgnoreCase(char.toString)) > 1 )aux.append(")")
      else aux.append("(")
    })
    aux.mkString("")

    // Your code here
  }
}
