package xieyuheng.cicada

case class Env(map: Map[String, Val] = Map()) {

  def lookup_val(name: String): Option[Val] =
    map.get(name)

  def ext_val(name: String, value: Val): Env =
    Env(map + (name -> value))

  def ext_decl(decl: Decl): Env = {
    val env = this
    decl match {
      case DeclLet(name, t, body) =>
        env.ext_val(name, eval(body, env))
      case DeclLetType(name, t) =>
        println(s"${name} is typed by undefined")
        throw new Exception()
      case DeclFn(name, args, dep_t, body) =>
        val fn = args.foldRight(body) { case ((arg_name, arg_t), body) => Fn(arg_name, arg_t, body) }
        env.ext_val(name, eval(fn, env))
      case DeclFnType(name, args, dep_t) =>
        println(s"${name} is typed by undefined")
        throw new Exception()
      case DeclClub(name, members, fields) =>
        val club_val = ValClub(name, members, Telescope.from_exp_fields(fields, env))
        val env2 = env.ext_val(name, club_val)
        members.foldLeft(env2) { case (env, member) =>
          val member_val = ValMember(
            member.name, name, Telescope.from_exp_fields(member.fields, env))
          env.ext_val(member.name, member_val) }
      case DeclRecord(name, super_names, decls) =>
        val record_val = ValRecord(name, super_names, Telescope.from_decls(decls, env))
        env.ext_val(name, record_val)
    }
  }

}
