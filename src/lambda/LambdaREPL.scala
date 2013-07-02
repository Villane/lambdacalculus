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
        case Success(expr, _) => println(pretty(eval(bind(expr))))
        case err: NoSuccess   => println(err)
      }
    }
  }
}
