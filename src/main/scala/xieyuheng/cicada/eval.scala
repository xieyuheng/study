package xieyuheng.cicada

import pretty._

object eval {

  def apply(exp: Exp, env: Env): Val =
    eval(exp: Exp, env: Env)

  def eval(exp: Exp, env: Env): Val = {
    exp match {
      case Var(name) =>
        env.lookup_val(name) match {
          case Some(value) =>
            value
          case None =>
            println(s"[eval fail]")
            println(s"undefined variable: ${name}")
            throw new Exception()
        }
      case Type(level) =>
        ValType(level)
      case Pi(arg_name, arg_t, dep_t) =>
        val arg_t_val = eval(arg_t, env)
        val dep_t_clo = Clo(arg_name, arg_t_val, dep_t, env)
        ValPi(arg_name, arg_t_val, dep_t_clo)
      case Fn(arg_name, arg_t, dep_t, body) =>
        val arg_t_val = eval(arg_t, env)
        val dep_t_clo = Clo(arg_name, arg_t_val, dep_t, env)
        val body_clo = Clo(arg_name, arg_t_val, body, env)
        ValFn(arg_name, arg_t_val, dep_t_clo, body_clo)
      case Ap(target, arg) =>
        ap_exe(eval(target, env), eval(arg, env))
      case Choice(path, map: Map[String, Exp]) =>
        choice_exe(path, map, env)
      case Dot(target, field_name) =>
        dot_exe(eval(target, env), field_name)
      case DotType(target, field_name) =>
        dot_type_exe(eval(target, env), field_name)
      case Let(decl, body) =>
        eval(body, env.ext_decl(decl))
    }
  }

  def eval_decl(decl: Decl, env: Env): Val = {
    decl match {
      case DeclLet(name, t, body) =>
        eval(body, env)
      case DeclLetType(name, t) =>
        println(s"${name} is typed by undefined")
        throw new Exception()
      case DeclFn(name, args, dep_t, body) =>
        val (pi, fn) = args.foldRight((dep_t, body)) { case ((arg_name, arg_t), (dep_t, body)) =>
          val pi = Pi(arg_name, arg_t, dep_t)
          val fn = Fn(arg_name, arg_t, dep_t, body)
          (pi, fn) }
        eval(fn, env)
      case DeclFnType(name, args, dep_t) =>
        println(s"${name} is typed by undefined")
        throw new Exception()
      case DeclClub(name, members, fields) =>
        val club_val = ValClub(name, members, Tel.from_exp_fields(fields, env))
        club_val
      case DeclRecord(name, super_names, decls) =>
        val record_val = ValRecord(name, super_names, Tel.from_decls(decls, env))
        record_val
    }
  }

  def ap_exe(target: Val, arg: Val): Val = {
    target match {
      case ValFn(arg_name: String, arg_t: Val, dep_t: Clo, body: Clo) =>
        body(arg)
      case ValClub(name, members, tel) =>
        ValClub(name, members, util.result_unwrap(tel.put(arg)))
      case ValMember(name, club_name, tel) =>
        ValMember(name, club_name, util.result_unwrap(tel.put(arg)))
      case ValRecord(name, super_names, tel) =>
        ValRecord(name, super_names, util.result_unwrap(tel.put(arg)))
      case neu: Neu =>
        NeuAp(neu, arg)
      case _ =>
        println(s"[eval fail]")
        println(s"can not apply ${target}")
        throw new Exception()
    }
  }

  def path_as_exp(path: List[String]): Exp = {
    assert(path.length > 0)
    val init: Exp = Var(path.head)
    path.tail.foldLeft(init) { case (exp, field_name) => Dot(exp, field_name) }
  }

  def choice_exe(path: List[String], map: Map[String, Exp], env: Env): Val = {
    val exp = Exp.from_path(path)
    val value = eval(exp, env)
    // TODO handle subtype relation in choice
    value match {
      case ValClub(name: String, members: List[Member], tel: Tel) =>
        map.get(name) match {
          case Some(body) => eval(body, env)
          case None =>
            println(s"[eval choice_exe]")
            println(s"choice mismatch: ${pretty_val(value)}")
            println(s"${pretty_exp_case(map)}")
            throw new Exception()
        }
      case ValMember(name: String, club_name: String, tel: Tel) =>
        map.get(name) match {
          case Some(body) => eval(body, env)
          case None =>
            println(s"[eval choice_exe]")
            println(s"choice mismatch: ${pretty_val(value)}")
            println(s"${pretty_exp_case(map)}")
            throw new Exception()
        }
      case ValRecord(name: String, super_names: List[String], tel: Tel) =>
        map.get(name) match {
          case Some(body) => eval(body, env)
          case None =>
            println(s"[eval choice_exe]")
            println(s"choice mismatch: ${pretty_val(value)}")
            println(s"${pretty_exp_case(map)}")
            throw new Exception()
        }
      case neu: Neu =>
        NeuChoice(neu, path, map, env)
      case _ =>
        println(s"[eval choice_exe]")
        println(s"choice mismatch: ${pretty_val(value)}")
        println(s"${pretty_exp_case(map)}")
        throw new Exception()
    }
  }

  def dot_exe(target: Val, field_name: String): Val = {
    target match {
      case ValClub(name, members, tel) =>
        tel.dot(field_name)
      case ValMember(name, club_name, tel) =>
        tel.dot(field_name)
      case ValRecord(name, super_names, tel) =>
        tel.dot(field_name)
      case neu: Neu =>
        NeuDot(neu, field_name)
      case _ =>
        println(s"[eval fail]")
        println(s"can not apply dot ${target}")
        throw new Exception()
    }
  }

  def dot_type_exe(target: Val, field_name: String): Val = {
    target match {
      case ValClub(name, members, tel) =>
        tel.dot_type(field_name)
      case ValMember(name, club_name, tel) =>
        tel.dot_type(field_name)
      case ValRecord(name, super_names, tel) =>
        tel.dot_type(field_name)
      case neu: Neu =>
        NeuDotType(neu, field_name)
      case _ =>
        println(s"[eval fail]")
        println(s"can not apply dot type ${target}")
        throw new Exception()
    }
  }

}
