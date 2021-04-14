import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object AssemblerInterpreter {
  def parseText(command: String): String = {
    val res = command.replaceAll("  ", " ")
      .replaceAll("   ", " ")
      .replaceAll("    ", " ")
      .replaceAll("     ", " ")
    res.replaceAll("^\\s+", "")
  }

  def interpret(input: String): Option[String] = {
    val program = input.split("\n")
    val variables: mutable.Map[String, Int] = mutable.Map()
    val parsedCommands: ArrayBuffer[String] = new ArrayBuffer[String]()
    var output = ""
    program.foreach(u => if (u.nonEmpty) parsedCommands.append(parseText(u)))
    var i = 0

    while (i < parsedCommands.length) {
      val command = parsedCommands(i).replace("  ", " ").split(" ")

      command(0) match {
        case "mov" =>
          val com1 = command(1).replace(",", "")
          val com2 = command(2).replace(",", "")

          if (com2.exists(_.isDigit)) variables.addOne((com1, com2.toInt))
          else variables.addOne((com1, variables(com2)))
          i += 1

        case "inc" =>
          val com1 = command(1).replace(",", "")

          variables(com1) += 1
          i += 1

        case "dec" =>
          val com1 = command(1).replace(",", "")

          variables(com1) -= 1
          i += 1

        case "add" =>
          val com1 = command(1).replace(",", "")
          val com2 = command(2).replace(",", "")

          if (com2.exists(_.isDigit)) {
            if (com2.toInt != 0) {
              variables(com1) = variables(com1) + com2.toInt
            }
          } else if (variables(com2) != 0) {
            variables(com1) = variables(com1) + variables(com2)
          }
          i += 1

        case "sub" =>
          val com1 = command(1).replace(",", "")
          val com2 = command(2).replace(",", "")

          if (com2.exists(_.isDigit)) {
            if (com2.toInt != 0) {
              variables(com1) = variables(com1) - com2.toInt
            }
          } else if (variables(com2) != 0) {
            variables(com1) = variables(com1) - variables(com2)
          }
          i += 1

        case "mul" =>
          val com1 = command(1).replace(",", "")
          val com2 = command(2).replace(",", "")

          if (com2.exists(_.isDigit)) {
            if (com2.toInt != 0) {
              variables(com1) = variables(com1) * com2.toInt
            }
          } else if (variables(com2) != 0) {
            variables(com1) = variables(com1) * variables(com2)
          }
          i += 1

        case "div" =>
          val com1 = command(1).replace(",", "")
          val com2 = command(2).replace(",", "")

          if (com2.exists(_.isDigit)) {
            if (com2.toInt != 0) {
              variables(com1) = variables(com1) / com2.toInt
            }
          } else if (variables(com2) != 0) {
            variables(com1) = variables(com1) / variables(com2)
          }
          i += 1

        case "jmp" =>
          // We store the name of the lbl in order to identify that this is a label and not a function
          i = parsedCommands.indexOf(command(1) + ":") + 1
          if(!variables.contains(command(1))) {
            variables.addOne(command(1), 0)
          }

        case "cmp" =>
          val com1 = command(1).replace(",", "")
          val com2 = command(2).replace(",", "")

          if (com1.exists(_.isDigit) && com2.exists(_.isDigit)) {
            if (com1.toInt >= com2.toInt) variables.addOne((command(0), 2))
            if (com1.toInt <= com2.toInt) variables.addOne((command(0), 1))
            if (com1.toInt == com2.toInt) variables.addOne((command(0), 0))
          } else if (!com1.exists(_.isDigit) && com2.exists(_.isDigit)) {
            if (variables(com1) >= com2.toInt) variables.addOne((command(0), 2))
            if (variables(com1) <= com2.toInt) variables.addOne((command(0), 1))
            if (variables(com1) == com2.toInt) variables.addOne((command(0), 0))
          } else if (com1.exists(_.isDigit) && !com2.exists(_.isDigit)) {
            if (com1.toInt >= variables(com2)) variables.addOne((command(0), 2))
            if (com1.toInt <= variables(com2)) variables.addOne((command(0), 1))
            if (com1.toInt == variables(com2)) variables.addOne((command(0), 0))
          } else if (!com1.exists(_.isDigit) && !com2.exists(_.isDigit)) {
            if (variables(com1) >= variables(com2)) variables.addOne((command(0), 2))
            if (variables(com1) <= variables(com2)) variables.addOne((command(0), 1))
            if (variables(com1) == variables(com2)) variables.addOne((command(0), 0))
          }
          i += 1

        case "jne" =>
          if (variables("cmp") > 0) {
            i = parsedCommands.indexOf(command(1) + ":") + 1
            if(!variables.contains(command(1))) {
              variables.addOne(command(1), 0)
            }
          } else i += 1

        case "je" =>
          if (variables("cmp") == 0) {
            i = parsedCommands.indexOf(command(1) + ":") + 1
            if(!variables.contains(command(1))) {
              variables.addOne(command(1), 0)
            }
          } else i += 1

        case "jge" =>
          if (variables("cmp") == 0 || variables("cmp") == 2) {
            i = parsedCommands.indexOf(command(1) + ":") + 1
            if(!variables.contains(command(1))) {
              variables.addOne(command(1), 0)
            }
          } else i += 1

        case "jg" =>
          if (variables("cmp") == 2) {
            i = parsedCommands.indexOf(command(1) + ":") + 1
            if(!variables.contains(command(1))) {
              variables.addOne(command(1), 0)
            }
          } else i += 1

        case "jle" =>
          if (variables("cmp") == 1 || variables("cmp") == 0) {
            i = parsedCommands.indexOf(command(1) + ":") + 1
            if(!variables.contains(command(1))) {
              variables.addOne(command(1), 0)
            }
          } else i += 1

        case "jl" =>
          if (variables("cmp") == 1) {
            i = parsedCommands.indexOf(command(1) + ":") + 1
            if(!variables.contains(command(1))) {
              variables.addOne(command(1), 0)
            }
          } else i += 1

        case "call" =>
          i = parsedCommands.indexOf(command(1) + ":") + 1
          variables.addOne(command(1), 1)

        case "ret" =>
          var j = i
          while (j >= 0) {
            if (parsedCommands(j).contains(":") && !parsedCommands(j).contains("msg")) {
              val nameFun = parsedCommands(j).replace(":", "")
              if (variables(nameFun) == 1) {
                i = parsedCommands.indexOf("call " + nameFun) + 1
                j = -1
              } else j -= 1
            }
            else j -= 1
          }

        case "msg" =>
          // Remove msg and comments
          var msg = parsedCommands(i).substring(4)
          if (msg.contains(";")) {
            msg = msg.substring(0, msg.indexOf(";"))
          }


          val theMatches = msg.split(",")
          var coma = false
          theMatches.foreach(u => {
            var msg2 = u
            if (msg2.nonEmpty) {
              if (msg2.replace(" ", "") == "'") {
                if (!coma) {
                  output = output + ", "
                  coma = true
                } else coma = false
              }
              else if (msg2.contains("'")) {
                val msg3 = msg2.substring(msg2.indexOf("'") + 1, msg2.lastIndexOf("'"))
                output = output + msg3
              } else {
                output = output + variables(msg2.replace(" ", "")).toString
              }
            }
          })
          i += 1

        case "end" =>
          if (output.nonEmpty) return Option(output)
          else return None

        case ";" =>
          i += 1

        case _ =>i += 1
      }
    }
    None
  }

}
