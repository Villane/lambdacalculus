package lambda
package eval

class Evaluation(debug: Boolean = false) {

  lazy val pretty = new PrettyPrinter()

  def apply(term: Expr): Expr =
    try {
      val term2 = evalStep(term)
      if (debug)
        println(s"step: ${pretty(term)} → ${pretty(term2)}")
      apply(term2)
    } catch {
      case _: MatchError => term
    }

  def evalStep(term: Expr): Expr = term match {
    case Apply(Lambda(argDef, body), arg) if isValue(arg) =>
      new Substitution(argDef, arg)(body)
    case Apply(fun, arg) if isValue(fun) =>
      Apply(fun, evalStep(arg))
    case Apply(fun, arg) =>
      Apply(evalStep(fun), arg)
  }

  def isValue(term: Expr): Boolean = term match {
    case _: Lambda  => true
    case _: Var     => true
    case _          => false
  }

}
