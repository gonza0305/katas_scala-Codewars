
// link : https://www.codewars.com/kata/5264d2b162488dc400000001

object Codewars {
    def spinWords(sentence: String): String = {
      val s1 = sentence.split(" ")
      val s2 = s1.map(u => {
        if(u.length >=5){
          u.reverse
        }else{
          u
        }
      })
      s2.mkString(" ")
    }
}
