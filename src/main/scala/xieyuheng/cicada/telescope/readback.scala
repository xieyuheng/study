package xieyuheng.cicada.telescope

object readback {

  type Seed = Int

  def init_seed(): Seed = 0

  def seed_inc(seed: Seed): Seed = seed + 1

  def fresh_name(seed: Seed): String = {
    s"#${seed}"
  }

  def gen_neu_val(arg_name: String, arg_t: Val, aka: Option[String] = None): NeuVar = {
    NeuVar(arg_name, arg_t, aka)
  }

  def gen_fresh(seed: Seed, arg_t: Val, aka: Option[String] = None): NeuVar = {
    gen_neu_val(fresh_name(seed), arg_t, aka)
  }

  def readback_val(seed: Seed, value: Val): Norm = {
    value match {
      case neu: Neu =>
        readback_neu(seed, neu)
      case ValType(level: Int) =>
        NormType(level)
      case ValPi(arg_name: String, arg_t: Val, dep_t: Clo) =>
        val arg_name = fresh_name(seed)
        val norm_arg_t = readback_val(seed, arg_t)
        NormPi(
          arg_name,
          norm_arg_t,
          readback_val(seed_inc(seed), dep_t(gen_fresh(seed, arg_t, Some(arg_name)))))
      case ValFn(arg_name: String, arg_t: Val, dep_t: Clo, body: Clo) =>
        val arg_name = fresh_name(seed)
        val norm_arg_t = readback_val(seed, arg_t)
        NormFn(
          arg_name,
          norm_arg_t,
          readback_val(seed_inc(seed), dep_t(gen_fresh(seed, arg_t, Some(arg_name)))),
          readback_val(seed_inc(seed), body(gen_fresh(seed, arg_t, Some(arg_name)))))
      case ValClub(name: String, members: List[Member], tel: Tel) =>
        NormClub(name, members, readback_tel(seed, tel))
      case ValMember(name: String, club_name: String, tel: Tel) =>
        NormMember(name, club_name, readback_tel(seed, tel))
      case ValRecord(name: String, super_names: List[String], tel: Tel) =>
        NormRecord(name, super_names, readback_tel(seed, tel))
    }
  }

  def readback_tel(seed: Seed, tel: Tel): NormTel = {
    val norm_fields = tel.fields.map {
      case (k, te, mve, None, mvv) =>
        (k, te, mve,
          readback_val(seed, eval(te, tel.env)),
          mvv.map(readback_val(seed, _)) )
      case (k, te, mve, Some(tv), mvv) =>
        (k, te, mve,
          readback_val(seed, tv),
          mvv.map(readback_val(seed, _)) ) }
    NormTel(norm_fields, seed, tel.env)
  }

  def readback_neu(seed: Seed, neu: Neu): NormNeu = {
    neu match {
      case NeuVar(name: String, arg_t: Val, aka: Option[String]) =>
        NormNeuVar(name, readback_val(seed, arg_t))
      case NeuAp(target: Neu, arg: Val) =>
        NormNeuAp(readback_neu(seed, target), readback_val(seed, arg))
      case NeuChoice(target: Neu, path: List[String], map: Map[String, Exp], env: Env) =>
        NormNeuChoice(readback_neu(seed, target), path, map, seed, env)
      case NeuDot(target: Neu, field_name: String) =>
        NormNeuDot(readback_neu(seed, target), field_name)
      case NeuDotType(target: Neu, field_name: String) =>
        NormNeuDotType(readback_neu(seed, target), field_name)
    }
  }

}
