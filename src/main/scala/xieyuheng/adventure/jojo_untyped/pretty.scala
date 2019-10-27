package xieyuheng.adventure.jojo_untyped

import xieyuheng.util.pretty._

object pretty {

  def pretty_jo(jo: Jo): String = {
    jo match {
      case Var(name: String) =>
        name
      case Let(name: String) =>
        s"[${name}]"
      case JoJo(list: List[Jo]) =>
        if (list.length == 0) {
          s"{ }"
        } else {
          s"{ ${pretty_jo_list(list)} }"
        }
      case Define(name: String, jojo: JoJo) =>
        s"${name} = ${pretty_jo(jojo)}"
      case Execute() =>
        s"exe"
      case Str(str) =>
        val doublequote = '"'
        s"${doublequote}${str}${doublequote}"
      case Cons() =>
        s"cons"
      case Car() =>
        s"car"
      case Cdr() =>
        s"cdr"
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
    }
  }

  def pretty_jo_list(list: List[Jo]): String = {
    list.map { case jo => pretty_jo(jo) }.mkString(" ")
  }

  def pretty_val(value: Val): String = {
    value match {
      case ValJoJo(list: List[Jo], env: Env) =>
        s"{ ${pretty_jo_list(list)} }"
      case ValStr(str) =>
        val doublequote = '"'
        s"${doublequote}${str}${doublequote}"
      case ValCons(car, cdr) =>
        s"cons(${pretty_val(car)}, ${pretty_val(cdr)})"
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
