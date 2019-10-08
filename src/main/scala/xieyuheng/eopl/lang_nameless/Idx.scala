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
final case class IdxDiff(exp1: Idx, exp2: Idx) extends Idx
final case class IdxZeroP(exp1: Idx) extends Idx
final case class IdxIf(exp1: Idx, exp2: Idx, exp3: Idx) extends Idx
final case class IdxLet(name: String, exp1: Idx, body: Idx) extends Idx
final case class IdxFn(name: String, body: Idx) extends Idx
final case class IdxAp(target: Idx, arg: Idx) extends Idx
