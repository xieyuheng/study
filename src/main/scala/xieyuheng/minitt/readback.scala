package xieyuheng.minitt

object readback {

  def fresh_name(i: Int): String = {
    s"$$${i}"
  }

  def readback_val(i: Int, value: Val): Norm = {
    value match {
      case ValNeu(neu: Neu) =>
        readback_neu(i, neu)
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

  def readback_neu(i: Int, neu: Neu): NormNeu = {
    neu match {
      case NeuVar(name: String) =>
        NormNeuVar(name)
      case NeuAp(target: Neu, arg: Val) =>
        NormNeuAp(readback_neu(i, target), readback_val(i, arg))
      case NeuCar(target: Neu) =>
        NormNeuCar(readback_neu(i, target))
      case NeuCdr(target: Neu) =>
        NormNeuCdr(readback_neu(i, target))
      case NeuMat(target: Neu, CloMat(mats: Map[String, Exp], env: Env)) =>
        NormNeuMat(readback_neu(i, target), mats, readback_env(i, env))
    }
  }

  def readback_env(i: Int, env: Env): NormEnv = {
    env match {
      case EnvDecl(decl: Decl, rest: Env) =>
        NormEnvDecl(decl, readback_env(i, rest))
      case EnvPat(pat: Pat, value: Val, rest: Env) =>
        NormEnvPat(pat, readback_val(i, value), readback_env(i, rest))
      case EnvEmpty() =>
        NormEnvEmpty()
    }
  }

}
