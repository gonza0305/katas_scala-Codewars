object Sum {
  def getSum(a: Int, b: Int): Int = {
    if(a==b) return a
    if(a<b) (a to b).toList.sum
    else (b to a).toList.sum

    //Good luck!
  }
}
