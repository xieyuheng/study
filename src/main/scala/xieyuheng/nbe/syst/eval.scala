package xieyuheng.syst

object eval {

  def apply(exp: Exp, env: Env): Val =
    eval(exp: Exp, env: Env)

  def eval(exp: Exp, env: Env): Val = {
    exp match {
      case Var(name: String) =>
        env.lookup_val(name) match {
          case Some(value) => value
          case None =>
            println(s"can not find var: ${this} in env")
            throw new Exception()
        }
      case NatRec(t: Type, target: Exp, base: Exp, step: Exp) =>
        exe_nat_rec(
          t,
          eval(target, env),
          eval(base, env),
          eval(step, env))
      case The(t: Type, exp: Exp) => eval(exp, env)
      case Succ(prev: Exp) => ValSucc(eval(prev, env))
      case Zero() => ValZero()
      case Ap(rator: Exp, arg: Exp) =>
        exe_ap(
          eval(rator, env),
          eval(arg, env))
      case Fn(name: String, body: Exp) => ValFn(name, body, env)
      case Block(DeclLet(name, t, e), body) =>
        eval(body, env.ext(name, eval(e, env)))
    }
  }

  def exe_ap(fn: Val, arg: Val): Val = {
    fn match {
      case ValFn(name, body, env) =>
        eval(body, env.ext(name, arg))
      case TheNeu(theType, neu) =>
        theType match {
          case Arrow(arg_t, dep_t) =>
            TheNeu(dep_t, NeuAp(neu, TheVal(arg_t, arg)))
          case _ =>
            println(s"type of neu fn is not Arrow: ${fn}")
            throw new Exception()
        }
      case _ =>
        println(s"fn is not a function: ${fn}")
        throw new Exception()
    }
  }

  def exe_nat_rec(t: Type, target: Val, base: Val, step: Val): Val = {
    target match {
      case ValZero() => base
      case ValSucc(prev) =>
        exe_ap(
          exe_ap(step, prev),
          exe_nat_rec(t, prev, base, step))
      case TheNeu(theType, neu) =>
        theType match {
          case Nat() =>
            TheNeu(t, NeuNatRec(
              t,
              neu,
              TheVal(t, base),
              TheVal(Arrow(Nat(), Arrow(t, t)), step)))
          case _ =>
            println(s"type of the neu target is not Nat: ${theType}")
            throw new Exception()
        }
      case _ =>
        println(s"target of NatRec is not ValZero() or ValSucc: ${target}")
        throw new Exception()
    }
  }

}
