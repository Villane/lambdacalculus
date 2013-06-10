package lambda

object LambdaREPL {

  def main(args: Array[String]) {
    val parser = new LambdaParser()
    import parser.{Success, NoSuccess}
    parser.parse("λx.x") match {
      case Success(expr, _) => println("Parsed: " + expr)
      case err: NoSuccess   => println(err)
    }
  }

}
