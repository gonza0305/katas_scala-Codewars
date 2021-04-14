
// LINK : https://www.codewars.com/kata/59df2f8f08c6cec835000012

object Meeting {

  def meeting(s: String): String = {
    val upperCase = s.toUpperCase
    val list = upperCase.split(";")
    val list2 = list.map(data => (data.split(":")(0),data.split(":")(1)))
    val list3 = list2.sortBy(data => (data._2,data._1))
    val list4 = list3.map(d=>(d._2," " + d._1))
    list4.mkString("")
    // your code
  }
}
