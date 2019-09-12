package xieyuheng.minitt

object exe {

  def ap_clo(clo: Clo, arg: Val): Val = {
    clo match {
      case CloFn(pat: Pat, body: Exp, env: Env) =>
        eval(body, EnvPat(pat, arg, env))
      case clo_mat @ CloMat(mats: Map[String, Exp], env: Env) =>
        arg match {
          case ValData(tag, body) =>
            mats.get(tag) match {
              case Some(exp) => exe.ap_val(eval(exp, env), body)
              case None => throw new Exception()
            }
          case ValNeu(target) => ValNeu(NeuMat(target, clo_mat))
          case _ => throw new Exception()
        }
    }
  }

  def ap_val(f: Val, arg: Val): Val = {
    f match {
      case ValFn(clo) => ap_clo(clo, arg)
      case ValMat(clo) => ap_clo(clo, arg)
      case ValNeu(target) => ValNeu(NeuAp(target, arg))
      case _ => throw new Exception()
    }
  }

}
