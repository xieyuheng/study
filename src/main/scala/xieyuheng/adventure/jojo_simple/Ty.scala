package xieyuheng.adventure.jojo_simple

sealed trait Ty
final case class TyAtom(name: String) extends Ty
final case class TyTy(list: List[Ty]) extends Ty
final case class TyCut() extends Ty
final case class TyMinus(t: Ty) extends Ty
final case class TyAssertEq() extends Ty
final case class TyPrint() extends Ty
