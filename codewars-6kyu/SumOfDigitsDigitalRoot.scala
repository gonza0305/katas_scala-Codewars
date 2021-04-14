
// LINK : https://www.codewars.com/kata/541c8630095125aba6000c00

object SumOfDigits {

  def digitalRoot(n: Int): Int = {
    if(n.toString.length<=1) n
    else{
      digitalRoot(n.toString.split("").map(n=>n.toInt).sum)
    }
  }
}
