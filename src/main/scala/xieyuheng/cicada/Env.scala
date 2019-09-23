package xieyuheng.cicada

import scala.annotation.tailrec

sealed trait Env {

  @tailrec
  def lookup_val(key: String): Option[Val] = {
    val env = this
    env match {
      case EnvEmpty() => None
      case EnvVal(name, value, rest) =>
        if (key == name) {
          Some(value)
        } else {
          rest.lookup_val(key)
        }
      case EnvDecl(decl, rest) =>
        decl match {
          case DeclLet(name, t, body) =>
            if (key == name) {
              Some(eval(body, env))
            } else {
              rest.lookup_val(key)
            }
          case DeclLetType(name, t) =>
            if (key == name) {
              println(s"${name} is typed by undefined")
              throw new Exception()
            } else {
              rest.lookup_val(key)
            }
          case DeclFn(name, args, dep_t, body) =>
            if (key == name) {
              val fn = args.foldRight(body) {
                case ((arg_name, arg_t), body) =>
                  Fn(arg_name, arg_t, body) }
              Some(eval(fn, env))
            } else {
              rest.lookup_val(key)
            }
          case DeclFnType(name, args, dep_t) =>
            if (key == name) {
              println(s"${name} is typed by undefined")
              throw new Exception()
            } else {
              rest.lookup_val(key)
            }
          case DeclClub(name, members, fields) =>
            if (key == name) {
              val club_val = ValClub(name, members, Telescope.from_exp_fields(fields, env))
              Some(club_val)
            } else {
              lookup_members(key, members, env) match {
                case Some(value) => Some(value)
                case None => rest.lookup_val(key)
              }
            }
          case DeclRecord(name, super_names, decls) =>
            if (key == name) {
              val record_val = ValRecord(name, super_names, Telescope.from_decls(decls, env))
              Some(record_val)
            } else {
              rest.lookup_val(key)
            }
        }
    }
  }

  def lookup_members(key: String, members: List[Member], env: Env): Option[Val] = {
    members.find {
      case member => key == member.name
    } match {
      case Some(member) =>
        val member_val = ValMember(
          member.name, member.club_name, Telescope.from_exp_fields(member.fields, env))
        Some(member_val)
      case None => None
    }
  }

  def ext_val(name: String, value: Val): Env = {
    EnvVal(name, value, this)
  }

  def ext_decl(decl: Decl): Env = {
    EnvDecl(decl, this)
  }

}

final case class EnvVal(name: String, value: Val, rest: Env) extends Env
final case class EnvDecl(decl: Decl, rest: Env) extends Env
final case class EnvEmpty() extends Env

object Env {
  def apply(): Env = EnvEmpty()
}
