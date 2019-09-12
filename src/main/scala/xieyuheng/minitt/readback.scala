package xieyuheng.minitt

object readback {

  def fresh_name(i: Int): String = {
    s"$$${i}"
  }

  def readback_val(i: Int, value: Val): Norm = {
    value match {
      case ValNeu(neu: Neu) =>
        NormNeu(readback_neu(i, neu))
      case ValFn(clo_fn: CloFn) =>
        val name = fresh_name(i)
        val body = readback_val(i + 1, exe.ap_clo(clo_fn, ValNeu(NeuVar(name))))
        NormFn(name, body)
      case ValPi(arg_t: Val, clo_fn: CloFn) =>
        val name = fresh_name(i)
        val t = readback_val(i + 1, exe.ap_clo(clo_fn, ValNeu(NeuVar(name))))
        NormPi(name, readback_val(i, arg_t), t)
      case ValSigma(arg_t: Val, clo_fn: CloFn) =>
        val name = fresh_name(i)
        val t = readback_val(i + 1, exe.ap_clo(clo_fn, ValNeu(NeuVar(name))))
        NormSigma(name, readback_val(i, arg_t), t)
      case ValUniv() => NormUniv()
      case ValCons(car: Val, cdr: Val) =>
        NormCons(readback_val(i, car), readback_val(i, cdr))
      case ValSole() => NormSole()
      case ValTrivial() => NormTrivial()
      case ValData(tag: String, body: Val) =>
        NormData(tag, readback_val(i, body))
      case ValSum(CloMat(mats: Map[String, Exp], env: Env)) =>
        NormMat(mats, readback_env(i, env))
      case ValMat(CloMat(mats: Map[String, Exp], env: Env)) =>
        NormSum(mats, readback_env(i, env))
    }
  }

  def readback_neu(i: Int, neu: Neu): NeuNorm = {
    neu match {
      case NeuVar(name: String) => ???
      case NeuAp(target: Neu, arg: Val) => ???
      case NeuCar(target: Neu) => ???
      case NeuCdr(target: Neu) => ???
      case NeuMat(target: Neu, clo_mat: CloMat) => ???
    }
  }

  def readback_env(i: Int, env: Env): NormEnv = {
    env match {
      case EnvDecl(decl: Decl, rest: Env) => ???
      case EnvPat(pat: Pat, value: Val, rest: Env) => ???
      case EnvEmpty() => ???
    }
  }

}
