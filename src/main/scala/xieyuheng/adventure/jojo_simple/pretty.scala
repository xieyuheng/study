package xieyuheng.adventure.jojo_simple

import xieyuheng.util.pretty._

object pretty {

  def pretty_jo(jo: Jo): String = {
    jo match {
      case Var(name: String) =>
        name
      case Let(name: String, t: Ty) =>
        s"(let ${name} : ${pretty_ty(t)})"
      case JoJo(list: List[Jo]) =>
        if (list.length == 0) {
          s"{ }"
        } else {
          s"{ ${pretty_jo_list(list)} }"
        }
      case Claim(name: String, tyty: TyTy) =>
        s"${name} : ${pretty_ty(tyty)}"
      case Define(name: String, jojo: JoJo) =>
        s"${name} = ${pretty_jo(jojo)}"
      case Execute() =>
        s"exe"
      case AssertEq() =>
        s"assert_eq"
      case ReportDs() =>
        s"report_ds"
      case ReportRs() =>
        s"report_rs"
      case Print() =>
        s"print"
      case Newline() =>
        s"ln"
      case Atom(name: String, str: String) =>
        val doublequote = '"'
        s"(atom ${name} ${doublequote}${str}${doublequote})"
    }
  }

  def pretty_jo_list(list: List[Jo]): String = {
    list.map { case jo => pretty_jo(jo) }.mkString(" ")
  }

  def pretty_ty(t: Ty): String = {
    t match {
      case TyAtom(name: String) =>
        name
      case TyTy(list: List[Ty]) =>
        if (list.length == 0) {
          s"{ }"
        } else {
          s"{ ${pretty_ty_list(list)} }"
        }
      case TyCut() =>
        s"cut"
      case TyMinus(t: Ty) =>
        s"(- ${pretty_ty(t)})"
      case TyAssertEq() =>
        s"ty_assert_eq"
      case TyPrint() =>
        s"ty_print"
    }
  }

  def pretty_ty_list(list: List[Ty]): String = {
    list.map { case t => pretty_ty(t) }.mkString(" ")
  }

  def pretty_val(value: Val): String = {
    value match {
      case ValJoJo(list: List[Jo], env: Env, ctx: Ctx) =>
        s"{ ${pretty_jo_list(list)} }"
      case ValAtom(name, str) =>
        val doublequote = '"'
        s"(atom ${name} ${doublequote}${str}${doublequote})"
    }
  }

  def pretty_ds(ds: Ds): String = {
    val s = ds.list.reverse.map { case value => pretty_val(value) }.mkString(" ")
    s"  * ${ds.length} *  -- ${s} --"
  }

  def pretty_frame(frame: Frame): String = {
    val s = frame.list.zipWithIndex.map { case (jo, index) =>
      if (index == frame.index) {
        s"@ ${pretty_jo(jo)}"
      } else {
        s"${pretty_jo(jo)}"
      }
    }.mkString(" ")
    s"> { ${s} }"
  }

  def pretty_rs(rs: Rs): String = {
    rs.list.reverse.map { case frame => pretty_frame(frame) }.mkString("\n")
  }

}
