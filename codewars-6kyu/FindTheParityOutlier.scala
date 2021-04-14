
// LINK : https://www.codewars.com/kata/5526fc09a1bbd946250002dc
object Parity {

  def findOutlier(integers: List[Int]): Int = {
    if(isEven(integers.head) && isEven(integers(1))) return integers.filter(x => !isEven(x)).head
    if(!isEven(integers.head) && !isEven(integers(1))) integers.filter(x => isEven(x)).head
    else {
        if(isEven(integers(2))) integers.filter(x => !isEven(x)).head
        else integers.filter(x => isEven(x)).head
    }
  }
  def isEven(n: Long): Boolean = n%2==0
}
