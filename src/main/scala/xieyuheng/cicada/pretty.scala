package xieyuheng.cicada

import xieyuheng.util.pretty._

object pretty {

  def pretty_decl(decl: Decl): String = {
    decl match {
      case DeclLet(name: String, t: Exp, body: Exp) =>
        s"let ${name}: ${pretty_exp(t)} = ${pretty_exp(body)}"
      case DeclLetType(name: String, t: Exp) =>
        s"let ${name}: ${pretty_exp(t)}"
      case DeclFn(name: String, args: Map[String, Exp], dep_t: Exp, body: Exp) =>
        val args_str = args.map {
          case (name, exp) =>
            s"${name}: ${pretty_exp(exp)}"
        }.mkString(", ")
        s"fn ${name}(${args_str}): ${pretty_exp(dep_t)} = ${pretty_exp(body)}"
      case DeclFnType(name: String, args: Map[String, Exp], dep_t: Exp) =>
        val args_str = args.map {
          case (name, exp) =>
            s"${name}: ${pretty_exp(exp)}"
        }.mkString(", ")
        s"fn ${name}(${args_str}): ${pretty_exp(dep_t)}"
      case DeclClub(name: String, members: List[Member], fileds: List[(String, Exp, Option[Exp])]) =>
        val fileds_str = fileds.map {
          case (name, t, None) =>
            s"${name}: ${pretty_exp(t)}"
          case (name, t, Some(e)) =>
            s"${name}: ${pretty_exp(t)} = ${pretty_exp(e)}"
        }.mkString(", ")
        val member_str = members.map(pretty_member).mkString("\n")
        s"data ${name}(${fileds_str}) {${maybeln(member_str)}}"
      case DeclRecord(name: String, super_names: List[String], decls: List[Decl]) =>
        val decls_str = decls.map(pretty_decl).mkString("\n")
        if (super_names.length == 0) {
          s"class ${name} {${maybeln(decls_str)}}"
        } else {
          val super_names_str = super_names.mkString(", ")
          s"class ${name} extends ${super_names_str} {${maybeln(decls_str)}}"
        }
    }
  }

  def pretty_member(member: Member): String = {
    val fileds = member.fileds.map {
      case (name, t, None) =>
        s"${name}: ${pretty_exp(t)}"
      case (name, t, Some(e)) =>
        s"${name}: ${pretty_exp(t)} = ${pretty_exp(e)}"
    }.mkString(", ")
    s"case ${member.name}(${fileds})"
  }

  def pretty_exp(exp: Exp): String = {
    exp match {
      case Var(name: String) =>
        name
      case Type(level: Int) =>
        if (level == 1) s"type_t" else s"type_t^${level}"
      case The(t: Exp, body: Exp) =>
        s"the(${pretty_exp(t)}, ${pretty_exp(body)})"
      case Pi(arg_name: String, arg_t: Exp, dep_t: Exp) =>
        s"(${arg_name}: ${pretty_exp(arg_t)}) -> ${pretty_exp(dep_t)}"
      case Fn(arg_name: String, arg_t: Exp, body: Exp) =>
        s"(${arg_name}: ${pretty_exp(arg_t)}) => ${pretty_exp(body)}"
      case Ap(target: Exp, arg: Exp) =>
        s"${pretty_exp(target)}(${pretty_exp(arg)})"
      case Choice(target: Exp, map: Map[String, Exp]) =>
        s"choice ${pretty_exp(target)} {${maybeln(pretty_exp_case(map))}}"
      case Dot(target: Exp, field_name: String) =>
        s"${pretty_exp(target)}.${field_name}"
      case DotType(target: Exp, field_name: String) =>
        s"${pretty_exp(target)}.:${field_name}"
      case Let(decl: Decl, body: Exp) =>
        s"{ ${pretty_decl(decl)}; ${pretty_exp(body)} }"
    }
  }

  def pretty_exp_case(map: Map[String, Exp]) =
    pretty_map(map) {
      case (name, exp) =>
        s"case ${name} => ${pretty_exp(exp)}\n" }

  def pretty_val(value: Val): String = {
    value match {
      case ValType(level) =>
        if (level == 1) s"type_t" else s"type_t^${level}"
      case ValThe(t: Val, body: Val) =>
        s"the(${pretty_val(t)}, ${pretty_val(body)})"
      case ValPi(arg_name: String, arg_t: Val, dep_t: Clo) =>
        s"(${arg_name}: ${pretty_val(arg_t)}) -> ${pretty_clo(dep_t)}"
      case ValFn(arg_name: String, arg_t: Val, body: Clo) =>
        s"(${arg_name}: ${pretty_val(arg_t)}) => ${pretty_clo(body)}"
      case ValClub(name: String, members: List[Member], tel: Telescope) =>
        s"${name}(${pretty_tel(tel)})"
      case ValMember(name: String, club_name: String, tel: Telescope) =>
        s"${name}(${pretty_tel(tel)})"
      case ValRecord(name: String, super_names: List[String], tel: Telescope) =>
        s"${name}(${pretty_tel(tel)})"
      case neu: Neu =>
        pretty_nen(neu)
    }
  }

  def pretty_val_case(map: Map[String, Val]) =
    pretty_map(map) {
      case (name, value) =>
        s"case ${name} => ${pretty_val(value)}\n" }

  def pretty_nen(neu: Neu): String = {
    neu match {
      case NeuVar(name: String) =>
        name
      case NeuAp(target: Neu, arg: Val) =>
        s"${pretty_nen(target)}(${pretty_val(arg)})"
      case NeuChoice(target: Neu, map: Map[String, Val]) =>
        s"choice ${pretty_nen(target)} {${maybeln(pretty_val_case(map))}}"
      case NeuDot(target: Neu, field_name: String) =>
        s"${pretty_nen(target)}.${field_name}"
      case NeuDotType(target: Neu, field_name: String) =>
        s"${pretty_nen(target)}.:${field_name}"
    }
  }

  def pretty_clo(clo: Clo): String = {
    s"#clo(${clo.arg_name}, ${pretty_exp(clo.body)})"
  }

  def pretty_tel(tel: Telescope): String = {
    val fileds = tel.fileds.map {
      case (name, t, None) =>
        s"${name}: ${pretty_exp(t)}"
      case (name, t, Some(e)) =>
        s"${name}: ${pretty_exp(t)} = ${pretty_exp(e)}"
    }.mkString(", ")

    val fills = tel.fills.map {
      case (name, t, v) =>
        s"${name}: ${pretty_val(t)} = ${pretty_val(v)}"
    }.mkString(", ")

    s"#tel(${fileds})(${fills})"
  }

}
