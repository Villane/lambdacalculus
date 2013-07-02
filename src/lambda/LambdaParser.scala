package lambda

import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.PackratParsers

class LambdaParser extends StdTokenParsers with PackratParsers {
  type Tokens = StdLexical
  val lexical = new LambdaLexer
  lexical.delimiters ++= Seq("\\", "λ", ".", "(", ")", "\\")

  type P[+T] = PackratParser[T]
  lazy val expr: P[Expr]         = application | notApp
  lazy val notApp                = variable | parens | lambda
  lazy val lambda: P[Lambda]     = ("λ" | "\\") ~> variable ~ "." ~ expr ^^
                                   { case v ~ "." ~ e  => Lambda(v, e) }
  lazy val application: P[Apply] = expr ~ notApp ^^
                                   { case left ~ right => Apply(left, right) }
  lazy val variable: P[Var]      = ident ^^ Var.apply
  lazy val parens: P[Expr]       = "(" ~> expr <~ ")"

  def parse(str: String): ParseResult[Expr] = {
    val tokens = new lexical.Scanner(str)
    phrase(expr)(tokens)
  }
}

class LambdaLexer extends StdLexical {
  override def letter = elem("letter", c => c.isLetter && c != 'λ')
}
