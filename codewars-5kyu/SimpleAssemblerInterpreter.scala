
// LINK : https://www.codewars.com/kata/58e24788e24ddee28e000053

import SimpleAssembler.interpret
import scala.collection.mutable.Map

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object SimpleAssembler {
  def interpret(program: List[String]): Map[String, Int] = {
    println(program)

    val values: mutable.Map[String, Int] = mutable.Map()
    var i = 0
    while(i < program.length){
      val command = program(i).split(" ")
      command(0) match{
        case "mov" => {
          if(command(2).exists(_.isDigit)) values.addOne((command(1),command(2).toInt))
          else values.addOne((command(1),values(command(2))))
          i += 1
        }
        case "inc" => {
          values(command(1)) += 1
          i += 1
        }
        case "dec" => {
          values(command(1)) -= 1
          i += 1
        }
        case "jnz" => {
          if(command(1).exists(_.isDigit)) {
            if (command(1).toInt != 0) {
              i = i + command(2).toInt
            } else i += 1
          }else if(values(command(1)) != 0){
              i = i + command(2).toInt
            } else i += 1
          }
        case _ =>
      }
    }

    values
  }
}
