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

    bool   = λb. b T F; // magic values T & F are recognized as booleans
    number = λn. n S Z; // magic values S & Z are recognized as numbers

    pred_z = pair 0 0;
    pred_s = \p. pair (second p) (succ (second p));
    pred   = \n. first (n pred_s pred_z);

    iszero = λn. n (\x.false) true;
    eq     = λa.λb. and (iszero (a pred b)) (iszero (b pred a));

    fix  = λf.(λw.f (λv.w w v)) (λw.f (λv.w w v));
    fact = fix (λfct. λn. ((iszero n) (λx. 1) (λx.(mul n (fct (pred n))))) \o.o);
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