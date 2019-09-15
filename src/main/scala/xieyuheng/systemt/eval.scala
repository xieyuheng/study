package xieyuheng.systemt

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
      case RecNat(t: Type, target: Exp, base: Exp, step: Exp) =>
        exe_rec_nat(
          t,
          eval(target, env),
          eval(base, env),
          eval(step, env))
      case The(t: Type, exp: Exp) => eval(exp, env)
      case Succ(prev: Exp) => ValSucc(eval(prev, env))
      case Zero => ValZero
      case Ap(rator: Exp, rand: Exp) =>
        exe_ap(
          eval(rator, env),
          eval(rand, env))
      case Fn(name: String, body: Exp) => ValFn(env, name, body)
    }
  }

  def exe_ap(fn: Val, arg: Val): Val = {
    fn match {
      case ValFn(env, name, body) =>
        eval(body, env.ext(name, arg))
      case TheNeu(theType, neu) =>
        theType match {
          case Arrow(argType, retType) =>
            TheNeu(retType, NeuAp(neu, TheVal(argType, arg)))
          case _ =>
            println(s"type of neu fn is not Arrow: ${fn}")
            throw new Exception()
        }
      case _ =>
        println(s"fn is not a function: ${fn}")
        throw new Exception()
    }
  }

  def exe_rec_nat(t: Type, target: Val, base: Val, step: Val): Val = {
    target match {
      case ValZero => base
      case ValSucc(prev) =>
        exe_ap(
          exe_ap(step, prev),
          exe_rec_nat(t, prev, base, step))
      case TheNeu(theType, neu) =>
        theType match {
          case Nat =>
            TheNeu(t, NeuRecNat(
              t,
              neu,
              TheVal(t, base),
              TheVal(Arrow(Nat, Arrow(t, t)), step)))
          case _ =>
            println(s"type of the neu target is not Nat: ${theType}")
            throw new Exception()
        }
      case _ =>
        println(s"target of RecNat is not ValZero or ValSucc: ${target}")
        throw new Exception()
    }
  }

}
