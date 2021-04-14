import scala.collection.mutable.ArrayBuffer

object GapInPrimes {
  def gap(g: Int, m: Long, n: Long): String = {
    val numbers_array =  (m to n).toArray
    numbers_array.foreach(number1 => {
      if(isPrime(number1.toInt) && isPrime((number1+g).toInt) && ((number1 +g) <= n )) {
        var flag = false
        val numbers_bet = (number1+1 to (number1+g-1)).toArray
        numbers_bet.foreach(number2 => {
          if(isPrime(number2.toInt)) flag = true
        })
        if(!flag) return s"("+number1+","+(number1+g)+")"
      }
    })
    ""
    // your code
  }
  def isPrime(i: Int): Boolean =
    if (i <= 1)
      false
    else if (i == 2)
      true
    else
      !(2 until i).exists(n => i % n == 0)
}
