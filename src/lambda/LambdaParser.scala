package lambda

import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.lexical.StdLexical

class LambdaParser extends StdTokenParsers {
  type Tokens = StdLexical
  val lexical = new StdLexical
  lexical.delimiters ++= Seq("λ", ".", "(", ")", "\\")

  def expr: Parser[Expr] = lambda | application | variable | parens
  def lambda             = "λ" ~> variable ~ "." ~ expr ^^ { case v ~ "." ~ e  => Lambda(v, e) }
  def application        = expr ~ expr                  ^^ { case left ~ right => Apply(left, right) }
  def variable           = ident                        ^^ Var
  def parens             = "(" ~> expr <~ ")"

  def parse(str: String): ParseResult[Expr] = {
    val tokens = new lexical.Scanner(str)
    phrase(expr)(tokens)
  }
}
