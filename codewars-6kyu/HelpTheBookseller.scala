
// LINK:  https://www.codewars.com/kata/54dc6f5a224c26032800005c

import scala.collection.mutable.ArrayBuffer

object StockList {
  def stockSummary(lstOfArt: Array[String], lstOfCat: Array[String]): String = {
    val aux2: ArrayBuffer[String] = new ArrayBuffer[String]()
    if(lstOfArt.isEmpty || lstOfCat.isEmpty) ""
    else{
      lstOfCat.foreach( cat => {
        val aux: ArrayBuffer[Int] = new ArrayBuffer[Int]()
        lstOfArt.foreach(elem => {
          if(elem.take(1).equals(cat)){

            val quant = elem.split(" ")(1).toInt
            aux.append(quant)

          }
        })
        if(aux2.isEmpty)aux2.append("("+cat+" "+":"+" "+aux.sum+")")
        else aux2.append(" - ("+cat+" "+":"+" "+aux.sum+")")
      })

      aux2.mkString("")
    }
  }
}
