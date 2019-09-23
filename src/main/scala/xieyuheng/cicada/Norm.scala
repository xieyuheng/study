package xieyuheng.cicada

sealed trait Norm
final case class NormVar(name: String) extends Norm
// TODO
