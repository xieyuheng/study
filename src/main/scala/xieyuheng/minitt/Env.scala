package xieyuheng.minitt

sealed trait Env
final case class DeclEnv(decl: Decl, rest: Env) extends Env
final case class PatternEnv(pattern: Pattern, value: Val, rest: Env) extends Env
final case class EmptyEnv() extends Env

object Env {
  def append(x: Env, y: Env): Env = {
    x match {
      case DeclEnv(decl, rest) =>
        DeclEnv(decl, append(rest, y))
      case PatternEnv(pattern, value, rest) =>
        PatternEnv(pattern, value, append(rest, y))
      case EmptyEnv() => y
    }
  }
}
