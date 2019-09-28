package xieyuheng.cicada

object infer {

  def apply(exp: Exp, env: Env): Val =
    infer(exp: Exp, env: Env)

  def infer(exp: Exp, env: Env): Val = {
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
        val arg_t_val = infer(arg_t, env)
        val dep_t_clo = Clo(arg_name, arg_t_val, dep_t, env)
        ValPi(arg_name, arg_t_val, dep_t_clo)
      case Fn(arg_name, arg_t, dep_t, body) =>
        val arg_t_val = infer(arg_t, env)
        val dep_t_clo = Clo(arg_name, arg_t_val, dep_t, env)
        val body_clo = Clo(arg_name, arg_t_val, body, env)
        // NOTE currently the only different from `eval`
        // ValFn(arg_name, arg_t_val, dep_t_clo, body_clo)
        ValPi(arg_name, arg_t_val, dep_t_clo)
      case Ap(target, arg) =>
        Ap.ap(infer(target, env), infer(arg, env))
      case Choice(path, map: Map[String, Exp]) =>
        Choice.ap(path, map, env)
      case Dot(target, field_name) =>
        Dot.ap(infer(target, env), field_name)
      case DotType(target, field_name) =>
        DotType.ap(infer(target, env), field_name)
      case Let(decl, body) =>
        infer(body, env.ext_decl(decl))
    }
  }

  def infer_decl(decl: Decl, env: Env): Val = {
    decl match {
      case DeclLet(name, t, body) =>
        infer(body, env)
      case DeclLetType(name, t) =>
        println(s"${name} is typed by undefined")
        throw new Exception()
      case DeclFn(name, args, dep_t, body) =>
        val (pi, fn) = args.foldRight((dep_t, body)) { case ((arg_name, arg_t), (dep_t, body)) =>
          val pi = Pi(arg_name, arg_t, dep_t)
          val fn = Fn(arg_name, arg_t, dep_t, body)
          (pi, fn) }
        infer(fn, env)
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
