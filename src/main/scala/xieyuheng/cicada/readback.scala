package xieyuheng.cicada

object readback {

  type Seed = Int

  def seed_inc(seed: Seed): Seed = seed + 1

  def fresh_name(seed: Seed): String = {
    s"#${seed}"
  }

  def readback_val(seed: Seed, value: Val): Norm = {
    ???
  }

  def readback_neu(seed: Seed, neu: Neu): NormNeu = {
    ???
  }

  def readback_env(seed: Seed, env: Env): NormEnv = {
    env match {
      case EnvDecl(decl: Decl, rest: Env) =>
        NormEnvDecl(decl, readback_env(seed, rest))
      case EnvVal(name, value: Val, rest: Env) =>
        NormEnvVal(name, readback_val(seed, value), readback_env(seed, rest))
      case EnvEmpty() =>
        NormEnvEmpty()
    }
  }

}
