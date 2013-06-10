package lambda

object LambdaREPL {
  val parser = new LambdaParser()
  val pretty = new PrettyPrinter()

  def main(args: Array[String]) {
    while (true) {
      val exprSrc = readLine("λ> ")
      import parser.{Success, NoSuccess}
      parser.parse(exprSrc) match {
        case Success(expr, _) => println("Parsed: " + pretty(expr))
        case err: NoSuccess   => println(err)
      }
    }
  }
}
