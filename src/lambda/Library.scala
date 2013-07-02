package lambda

object Library {

  val source = """
    id = \x.x;

    true  = λt. λf. t;
    false = λt. λf. f;
    if    = λc. λt. λe. c t e;
    or    = λa. λb. a true b;
    and   = λa. λb. a b false;

    pair   = λf. λs. λb. b f s;
    first  = λp. p true;
    second = λp. p false;

    succ = λn.λs.λz. s (n s z);
    add  = λa.λb.λs.λz. a s (b s z);
    mul  = λa.λb.λs. a (b s);
    pow  = λa.λb. b a;
  """

  def load() = {
    val parse = new LambdaParser()
    import parse.{ Success, NoSuccess }
    parse.definitions(source) match {
      case Success(lib, _)   => lib
      case NoSuccess(err, _) => println(err); Map[String, Expr]()
    }
  }

}