package xieyuheng.cicada

import xieyuheng.util.pretty._

object pretty {

  def pretty_level(level: Int): String = {
    if (level == 1) s"type_t" else s"type_t^${level}"
  }

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
      case DeclClub(name: String, members: List[Member], fields: List[(String, Exp, Option[Exp])]) =>
        val fields_str = fields.map {
          case (name, t, None) =>
            s"${name}: ${pretty_exp(t)}"
          case (name, t, Some(e)) =>
            s"${name}: ${pretty_exp(t)} = ${pretty_exp(e)}"
        }.mkString(", ")
        val member_str = members.map(pretty_member).mkString("\n")
        s"data ${name}(${fields_str}) {${maybeln(member_str)}}"
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
    val fields = member.fields.map {
      case (name, t, None) =>
        s"${name}: ${pretty_exp(t)}"
      case (name, t, Some(e)) =>
        s"${name}: ${pretty_exp(t)} = ${pretty_exp(e)}"
    }.mkString(", ")
    s"case ${member.name}(${fields})"
  }

  def pretty_exp(exp: Exp): String = {
    exp match {
      case Var(name: String) =>
        name
      case Type(level: Int) =>
        pretty_level(level)
      case Pi(arg_name: String, arg_t: Exp, dep_t: Exp) =>
        s"(${arg_name}: ${pretty_exp(arg_t)}) -> ${pretty_exp(dep_t)}"
      case Fn(arg_name: String, arg_t: Exp, body: Exp) =>
        s"(${arg_name}: ${pretty_exp(arg_t)}) => ${pretty_exp(body)}"
      case Ap(target: Exp, arg: Exp) =>
        s"${pretty_exp(target)}(${pretty_exp(arg)})"
      case Choice(path: List[String], map: Map[String, Exp]) =>
        s"choice ${pretty_path(path)} {${maybeln(pretty_exp_case(map))}}"
      case Dot(target: Exp, field_name: String) =>
        s"${pretty_exp(target)}.${field_name}"
      case DotType(target: Exp, field_name: String) =>
        s"${pretty_exp(target)}.:${field_name}"
      case Let(decl: Decl, body: Exp) =>
        s"{ ${pretty_decl(decl)}; ${pretty_exp(body)} }"
    }
  }

  def pretty_path(path: List[String]): String = {
    path.mkString(".")
  }

  def pretty_exp_case(map: Map[String, Exp]) =
    pretty_map(map) {
      case (name, exp) =>
        s"case ${name} => ${pretty_exp(exp)}\n" }

  def pretty_val(value: Val): String = {
    value match {
      case ValType(level) =>
        pretty_level(level)
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
        pretty_neu(neu)
    }
  }

  def pretty_val_case(map: Map[String, Val]) =
    pretty_map(map) {
      case (name, value) =>
        s"case ${name} => ${pretty_val(value)}\n" }

  def pretty_neu(neu: Neu): String = {
    neu match {
      case NeuVar(name: String, aka) =>
        aka match {
          case Some(alias) => s"${alias}${name}"
          case None => name
        }
      case NeuAp(target: Neu, arg: Val) =>
        s"${pretty_neu(target)}(${pretty_val(arg)})"
      case NeuChoice(target: Neu, map: Map[String, Exp], env) =>
        s"choice ${pretty_neu(target)} {${maybeln(pretty_exp_case(map))}}"
      case NeuDot(target: Neu, field_name: String) =>
        s"${pretty_neu(target)}.${field_name}"
      case NeuDotType(target: Neu, field_name: String) =>
        s"${pretty_neu(target)}.:${field_name}"
    }
  }

  def pretty_clo(clo: Clo): String = {
    s"#clo(${clo.arg_name}, ${pretty_exp(clo.body)})"
  }

  def pretty_tel(tel: Telescope): String = {
    val fields = tel.fields.map {
      case (k, te, ve, Some(tv), Some(vv)) =>
        s"${k}: ${pretty_val(tv)} = ${pretty_val(vv)}"
      case (k, te, _, _, _) =>
        s"${k}: ${pretty_exp(te)}"
    }.mkString(", ")
    s"${fields}"
  }

  def pretty_norm(norm: Norm): String = {
    norm match {
      case norm_neu: NormNeu =>
        pretty_norm_neu(norm_neu)
      case NormType(level: Int) =>
        pretty_level(level)
      case NormPi(arg_name: String, arg_t: Norm, dep_t: Norm) =>
        s"(${arg_name}: ${pretty_norm(arg_t)}) -> ${pretty_norm(dep_t)}"
      case NormFn(arg_name: String, arg_t: Norm, body: Norm) =>
        s"(${arg_name}: ${pretty_norm(arg_t)}) => ${pretty_norm(body)}"
      case NormClub(name: String, members: List[Member], norm_tel: NormTelescope) =>
        s"${name}(${pretty_norm_tel(norm_tel)})"
      case NormMember(name: String, club_name: String, norm_tel: NormTelescope) =>
        s"${name}(${pretty_norm_tel(norm_tel)})"
      case NormRecord(name: String, super_names: List[String], norm_tel: NormTelescope) =>
        s"${name}(${pretty_norm_tel(norm_tel)})"
    }
  }

  def pretty_norm_neu(norm_neu: NormNeu): String = {
    norm_neu match {
      case NormNeuVar(name: String) =>
        name
      case NormNeuAp(target: NormNeu, arg: Norm) =>
        s"${pretty_norm_neu(target)}(${pretty_norm(arg)})"
      case NormNeuChoice(target: NormNeu, map: Map[String, Exp], env: NormEnv) =>
        s"choice ${pretty_norm_neu(target)} {${maybeln(pretty_exp_case(map))}}"
      case NormNeuDot(target: NormNeu, field_name: String) =>
        s"${pretty_norm_neu(target)}.${field_name}"
      case NormNeuDotType(target: NormNeu, field_name: String) =>
        s"${pretty_norm_neu(target)}.:${field_name}"
    }
  }

  def pretty_norm_tel(norm_tel: NormTelescope): String = {
    val fields = norm_tel.fields.map {
      case (k, te, ve, Some(tn), Some(vn)) =>
        s"${k}: ${pretty_norm(tn)} = ${pretty_norm(vn)}"
      case (k, te, _, _, _) =>
        s"${k}: ${pretty_exp(te)}"
    }.mkString(", ")
    s"${fields}"
  }

}
