
// LINK : https://www.codewars.com/kata/55f2b110f61eb01779000053

object Sum {
  def getSum(a: Int, b: Int): Int = {
    if(a==b) return a
    if(a<b) (a to b).toList.sum
    else (b to a).toList.sum

    //Good luck!
  }
}
