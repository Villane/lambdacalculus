package lambda

class PrettyPrinter {
  def apply(expr: Expr): String = expr match {
    case Lambda(arg, body) => p"λ$arg.$body"
    case CNumber(i)        => i.toString
    case CBoolean(b)       => b.toString
    case Apply(fun, arg)   => p"$fun $arg"
    case Var(name, scope)  => s"$name"
  }

  implicit class PrettyPrinting(val sc: StringContext) {
    def p(args: Expr*) = sc.s((args map parensIfNeeded): _*)
  }

  def parensIfNeeded(expr: Expr) = expr match {
    case v: Var => apply(v)
    case _      => "(" + apply(expr) + ")"
  }
}
