package xieyuheng.eopl.lang_nameless

object tran {

  def tran_nameless(exp: Exp, idx_env: IdxEnv): Idx = {
    exp match {
      case Var(name: String) =>
        ???
      case Num(num: Int) =>
        ???
      case Diff(exp1: Exp, exp2: Exp) =>
        ???
      case ZeroP(exp1: Exp) =>
        ???
      case If(exp1: Exp, exp2: Exp, exp3: Exp) =>
        ???
      case Let(name: String, exp1: Exp, body: Exp) =>
        ???
      case Fn(name: String, body: Exp) =>
        ???
      case Ap(target: Exp, arg: Exp) =>
        ???
    }
  }

}
