package lambda

sealed trait Expr

case class Lambda(arg: Var, body: Expr) extends Expr

object Var {
  def apply(name: String): Var = Var(name, Scope.TOP)
}
case class Var(name: String, scope: Scope) extends Expr

case class Apply(fun: Expr, arg: Expr) extends Expr
