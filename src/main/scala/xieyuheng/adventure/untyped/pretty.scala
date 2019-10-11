package xieyuheng.adventure.untyped

import xieyuheng.util.pretty._

object pretty {

  def pretty_jo(jo: Jo): String = {
    jo match {
      case Var(name: String) =>
        name
      case Let(name: String) =>
        s"let ${name}"
      case JoJo(list: List[Jo]) =>
        s"{ ${pretty_jo_list(list)} }"
      case Define(name: String, jojo: JoJo) =>
        // s"${name} = ${pretty_jo(jojo)}"
        s"def ${name}"
      case Str(str) =>
        val doublequote = '"'
        s"${doublequote}${str}${doublequote}"
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
