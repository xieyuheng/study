package xieyuheng.minitt

object readback {

  def fresh_name(i: Int): String = {
    s"$$${i}"
  }

  def readback_val(i: Int, value: Val): Exp = {
    value match {
      case ValNeu(neu: Neu) =>
        readback_neu(i, neu)
      case ValFn(clo_fn: CloFn) =>
        val name = fresh_name(i)
        val body = readback_val(i + 1, exe.ap_clo(clo_fn, ValNeu(NeuVar(name))))
        Fn(PatVar(name), body)
      case ValPi(arg_t: Val, clo_fn: CloFn) =>
        val name = fresh_name(i)
        val t = readback_val(i + 1, exe.ap_clo(clo_fn, ValNeu(NeuVar(name))))
        Pi(PatVar(name), readback_val(i, arg_t), t)
      case ValSigma(arg_t: Val, clo_fn: CloFn) =>
        val name = fresh_name(i)
        val t = readback_val(i + 1, exe.ap_clo(clo_fn, ValNeu(NeuVar(name))))
        Sigma(PatVar(name), readback_val(i, arg_t), t)
      case ValUniv() => Univ()
      case ValCons(car: Val, cdr: Val) =>
        Cons(readback_val(i, car), readback_val(i, cdr))
      case ValSole() => Sole()
      case ValTrivial() => Trivial()
      case ValData(tag: String, body: Val) =>
        Data(tag, readback_val(i, body))
      case ValSum(clo_mat: CloMat) =>
        ???
        // exe.ap_clo(clo_mat, )
        // Mat(mats: Map[String, Exp])
      case ValMat(clo_mat: CloMat) =>
        ???
        // Sum(mats: Map[String, Exp])
    }
  }

  def readback_neu(i: Int, neu: Neu): Exp = {
    neu match {
      case NeuVar(name: String) => ???
      case NeuAp(target: Neu, arg: Val) => ???
      case NeuCar(target: Neu) => ???
      case NeuCdr(target: Neu) => ???
      case NeuMat(target: Neu, clo_mat: CloMat) => ???
    }
  }

  def readback_env(i: Int, env: Env): Env = {
    env match {
      case DeclEnv(decl: Decl, rest: Env) => ???
      case PatEnv(pat: Pat, value: Val, rest: Env) => ???
      case EmptyEnv() => ???
    }
  }

}
