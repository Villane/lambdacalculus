package lambda

import lambda.eval.Evaluation

object LambdaREPL {
  val parser = new LambdaParser()
  val pretty = new PrettyPrinter()
  var bind = new Binder(Library.load())
  val eval = new Evaluation(debug = false)

  def main(args: Array[String]) {
    while (true) {
      val input = readLine("λ> ")
      if (input contains "=")
        handleDef(input)
      else
        handleExpr(input)
    }
  }

  def handleDef(input: String) =
    parseInput(parser.definitions, input) { defs =>
      println("Defined: " + defs.keys.mkString(", "))
      bind = new Binder(bind.defs ++ defs)
    }

  def handleExpr(input: String) =
    parseInput(parser.parse, input) { expr =>
      val bound = bind(expr)
      if (bind.messages.isEmpty)
        println(pretty(eval(bound)))
      else {
        for (m <- bind.messages)
          println(m.pos.longString + m.msg)
        bind.messages.clear()
      }
    }

  def parseInput[T](p: String => parser.ParseResult[T], input: String)(success: T => Unit): Unit = {
    import parser.{ Success, NoSuccess }
    p(input) match {
      case Success(res, _) => success(res)
      case NoSuccess(err, _) => println(err)
    }
  }

}
