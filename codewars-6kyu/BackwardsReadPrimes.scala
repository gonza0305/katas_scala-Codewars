
// LINK : https://www.codewars.com/kata/5539fecef69c483c5a000015

object Solution {
  def backwardsPrime(start: Long, nd: Long): String =
    (start to nd).toList.filter(u=> isPrime(u.toString.reverse.toLong) && isPrime(u) && u.toString.reverse!=u.toString).mkString(",")

  def isPrime(n: Long): Boolean = !(2 +: (3 to Math.sqrt(n).toInt by 2) exists (n%_ == 0))
}
