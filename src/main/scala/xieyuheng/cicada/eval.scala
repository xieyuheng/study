package xieyuheng.cicada

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
            println(s"undefined variable: ${name}")
            throw new Exception()
        }
      case Type(level) =>
        ValType(level)
      case Pi(arg_name, arg_t, dep_t) =>
        val arg_t_val = eval(arg_t, env)
        ValPi(arg_name, arg_t_val, Clo(arg_name, arg_t_val, dep_t, env))
      case Fn(arg_name, arg_t, body) =>
        val arg_t_val = eval(arg_t, env)
        ValFn(arg_name, arg_t_val, Clo(arg_name, arg_t_val, body, env))
      case Ap(target, arg) =>
        Ap.ap(eval(target, env), eval(arg, env))
      case Choice(path, map: Map[String, Exp]) =>
        Choice.ap(path, map, env)
      case Dot(target, field_name) =>
        Dot.ap(eval(target, env), field_name)
      case DotType(target, field_name) =>
        DotType.ap(eval(target, env), field_name)
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
        val fn = args.foldRight(body) { case ((arg_name, arg_t), body) =>
          Fn(arg_name, arg_t, body) }
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

}
