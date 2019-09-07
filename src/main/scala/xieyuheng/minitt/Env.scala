package xieyuheng.minitt

sealed trait Env
final case class DeclEnv(decl: Decl, rest: Env) extends Env
final case class BindEnv(pattern: Pattern, value: Value, rest: Env) extends Env
