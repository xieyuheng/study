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
        s"${name} = ${pretty_jo(jojo)}"
    }
  }

  def pretty_jo_list(list: List[Jo]): String = {
    list.map { case jo => pretty_jo(jo) }.mkString(" ")
  }

  def pretty_val(value: Val): String = {
    value match {
      case ValJoJo(list: List[Jo], env: Env) =>
        s"{ ${pretty_jo_list(list)} }"
    }
  }

  def pretty_ds(ds: Ds): String = {
    val s = ds.list.reverse.map { case value => pretty_val(value) }.mkString(" ")
    s"  * ${ds.length} *  -- ${s} --"
  }

}
