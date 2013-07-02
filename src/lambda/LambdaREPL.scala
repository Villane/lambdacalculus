package lambda

import lambda.eval.Evaluation

object LambdaREPL {
  val parser = new LambdaParser()
  val pretty = new PrettyPrinter()
  val bind = new Binder()
  val eval = new Evaluation(debug = true)

  def main(args: Array[String]) {
    while (true) {
      val exprSrc = readLine("λ> ")
      import parser.{Success, NoSuccess}
      parser.parse(exprSrc) match {
        case Success(expr, _) =>
          val bound = bind(expr)
          if (bind.messages.isEmpty)
            println(pretty(eval(bound)))
          else {
            for (m <- bind.messages)
              println(m.pos.longString + m.msg)
            bind.messages.clear()
          }
        case err: NoSuccess   => println(err)
      }
    }
  }
}
