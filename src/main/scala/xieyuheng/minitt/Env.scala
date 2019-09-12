package xieyuheng.minitt

sealed trait Env
final case class EnvDecl(decl: Decl, rest: Env) extends Env
final case class EnvPat(pat: Pat, value: Val, rest: Env) extends Env
final case class EnvEmpty() extends Env

object Env {
  def append(x: Env, y: Env): Env = {
    x match {
      case EnvDecl(decl, rest) =>
        EnvDecl(decl, append(rest, y))
      case EnvPat(pat, value, rest) =>
        EnvPat(pat, value, append(rest, y))
      case EnvEmpty() => y
    }
  }
}
