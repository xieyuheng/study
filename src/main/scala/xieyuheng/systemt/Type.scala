package xieyuheng.systemt

sealed trait Type
final case object Nat extends Type
final case class Arrow(argType: Type, retType: Type) extends Type
