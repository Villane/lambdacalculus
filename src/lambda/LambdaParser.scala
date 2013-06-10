package lambda

import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.lexical.StdLexical

class LambdaParser extends StdTokenParsers {
  type Tokens = StdLexical
  val lexical = new StdLexical
  lexical.delimiters ++= Seq("λ", ".", "(", ")", "\\")

  def parse(str: String): ParseResult[String] = {
    val tokens = new lexical.Scanner(str)
    phrase(ident)(tokens)
  }
}
