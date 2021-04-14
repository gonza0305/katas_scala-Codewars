object SumOfDigits {

  def digitalRoot(n: Int): Int = {
    if(n.toString.length<=1) n
    else{
      digitalRoot(n.toString.split("").map(n=>n.toInt).sum)
    }
  }
}
