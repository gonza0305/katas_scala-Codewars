object FindTheOddInt {
  def findOdd(xs: Seq[Int]): Int = {
    //Group by their length
    xs.map(x=>x.toString).groupBy(identity).map(x=> (x._1.toInt,x._2.size)).filter(_._2%2 !=0).keys.head
  }
}
