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
