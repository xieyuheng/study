package xieyuheng.cicada

case class Env(map: Map[String, Val] = Map()) {

  def lookup_val(name: String): Option[Val] =
    map.get(name)

  def ext(name: String, value: Val): Env =
    Env(map + (name -> value))

  def ext_by_decl(decl: Decl): Env = {
    val env = this
    decl match {
      case DeclLet(name: String, t: Exp, body: Exp) =>
        env.ext(name, eval(body, env))
      case DeclLetType(name: String, t: Exp) =>
        println(s"${name} is typed by undefined")
        throw new Exception()
      case DeclFn(name: String, args: Map[String, Exp], dep_t: Exp, body: Exp) =>
        val exp = args.foldRight(body) {
          case ((arg_name, arg_t), body) =>
            Fn(arg_name, arg_t, body) }
        env.ext(name, eval(exp, env))
      case DeclFnType(name: String, args: Map[String, Exp], dep_t: Exp) =>
        println(s"${name} is typed by undefined")
        throw new Exception()
      case DeclClub(name: String, members: List[Member], fields: List[(String, Exp, Option[Exp])]) =>
        // ValClub(name: String, members: List[Member], tel: Telescope)
        // ValMember(name: String, club_name: String, tel: Telescope)
        // Member(name: String, club_name: String, fields: List[(String, Exp, Option[Exp])])
        // TODO
        ???
      case DeclRecord(name: String, super_names: List[String], decls: List[Decl]) =>
        val tel = Telescope.from_decls(decls, env)
        env.ext(name, ValRecord(name, super_names, tel))
    }
  }

}
