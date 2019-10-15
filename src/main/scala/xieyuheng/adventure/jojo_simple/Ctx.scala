package xieyuheng.adventure.jojo_simple

sealed trait CtxEntry
final case class CtxEntryLet(t: Ty) extends CtxEntry
final case class CtxEntryClaim(tyty: TyTy) extends CtxEntry

case class Ctx(map: Map[String, CtxEntry] = Map()) {

  def lookup(name: String): Option[CtxEntry] = {
    map.get(name)
  }

  def ext(name: String, entry: CtxEntry): Ctx = {
    Ctx(map + (name -> entry))
  }

}
