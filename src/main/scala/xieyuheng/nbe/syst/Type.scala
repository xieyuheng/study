package xieyuheng.syst

sealed trait Type
final case class Nat() extends Type
final case class Arrow(arg_t: Type, dep_t: Type) extends Type
