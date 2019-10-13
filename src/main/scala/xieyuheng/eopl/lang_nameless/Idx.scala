package xieyuheng.eopl.lang_nameless

sealed trait Idx
final case class IdxVar(name: String, index: Int) extends Idx {
  val matters = index

  override def equals(that: Any): Boolean = {
    that match {
      case that: IdxVar => this.matters == that.matters
      case _ => false
    }
  }

  override def hashCode = matters.hashCode
}
final case class IdxNum(num: Int) extends Idx
final case class IdxDiff(idx1: Idx, idx2: Idx) extends Idx
final case class IdxZeroP(idx1: Idx) extends Idx
final case class IdxIf(idx1: Idx, idx2: Idx, idx3: Idx) extends Idx
final case class IdxLet(name: String, idx1: Idx, body: Idx) extends Idx
final case class IdxFn(name: String, body: Idx) extends Idx
final case class IdxAp(target: Idx, arg: Idx) extends Idx
final case class IdxSole() extends Idx
final case class IdxDo(idx1: Idx, body: Idx) extends Idx
