package xieyuheng.cicada

import scala.annotation.tailrec

import pretty._

sealed trait Env {

  @tailrec
  def lookup_val(key: String): Option[Val] = {
    val env = this
    env match {
      case EnvEmpty() => None
      case EnvName(name, value, rest) =>
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
              val club_val = ValClub(name, members, Tel.from_exp_fields(fields, env))
              Some(club_val)
            } else {
              lookup_members(key, members, env) match {
                case Some(value) => Some(value)
                case None => rest.lookup_val(key)
              }
            }
          case DeclRecord(name, super_names, decls) =>
            if (key == name) {
              val record_val = ValRecord(name, super_names, Tel.from_decls(decls, env))
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
          member.name, member.club_name, Tel.from_exp_fields(member.fields, env))
        Some(member_val)
      case None => None
    }
  }

  def ext_val(name: String, value: Val): Env = {
    EnvName(name, value, this)
  }

  def ext_decl(decl: Decl): Env = {
    EnvDecl(decl, this)
  }

  def append(that: Env): Env = {
    this match {
      case EnvName(name, value, rest) =>
        EnvName(name, value, rest.append(that))
      case EnvDecl(decl, rest) =>
        EnvDecl(decl, rest.append(that))
      case EnvEmpty() =>
        that
    }
  }

  def ext_by_path(path: List[String], value: Val): Either[Err, Env] = {
    val env = this
    assert(path.length >= 1)
    env.lookup_val(path.head) match {
      case Some(src) =>
        for {
          new_value <- val_ext_by_path(src, path.tail, value)
        } yield env.ext_val(path.head, new_value)
      case None =>
        Left(Err(
          s"[ext_by_path fail]" ++
            s"path: ${pretty_path(path)}" ++
            s"value: ${pretty_val(value)}"))
    }
  }

  def val_ext_by_path(src: Val, path: List[String], value: Val): Either[Err, Val] = {
    path.headOption match {
      case Some(head) =>
        src match {
          case ValClub(name, members, tel) =>
            for {
              new_tel <- tel_ext_by_path(tel, head, path.tail, value)
            } yield ValClub(name, members, new_tel)
          case ValMember(name, club_name, tel) =>
            for {
              new_tel <- tel_ext_by_path(tel, head, path.tail, value)
            } yield ValMember(name, club_name, new_tel)
          case ValRecord(name, super_names, tel) =>
            for {
              new_tel <- tel_ext_by_path(tel, head, path.tail, value)
            } yield ValRecord(name, super_names, new_tel)
          case _ => Left(Err(
            s"[val_ext_by_path fail]" ++
              s"src: ${pretty_val(src)}" ++
              s"path: ${pretty_path(path)}" ++
              s"value: ${pretty_val(value)}"))
        }
      case None => Right(value)
    }
  }

  def tel_ext_by_path(
    tel: Tel,
    head: String,
    tail: List[String],
    value: Val,
  ): Either[Err, Tel] = {
    val fields = tel.fields
    val env = tel.env

    val i = fields.indexWhere {
      case (k, _, _, _, _) => k == head
    }

    if (i == -1) {
      Left(Err(
        s"[tel_ext_by_path fail]" ++
          s"tel: ${pretty_tel(tel)}" ++
          s"head: ${head}" ++
          s"tail: ${pretty_path(tail)}" ++
          s"value: ${pretty_val(value)}"))
    } else {
      val (k, te, mve, mtv, mvv) = fields(i)
      mvv match {
        case Some(vv) =>
          val_ext_by_path(vv, tail, value) match {
            case Right(arg) =>
              val new_fields = util.list_replace(fields, i,
                (k, te, mve, Some(eval(te, env)), Some(arg)))
              Right(
                Tel(new_fields, env.ext_val(k, arg))
                  .self_put())
            case Left(err) =>
              Left(err)
          }
        case None =>
          if (tail.length == 0) {
            val arg = value
            val new_fields = util.list_replace(fields, i,
              (k, te, mve, Some(eval(te, env)), Some(arg)))
            Right(
              Tel(new_fields, env.ext_val(k, arg))
                .self_put())
          } else {
            Left(Err(
              s"[tel_ext_by_path fail]" ++
                s"tel: ${pretty_tel(tel)}" ++
                s"head: ${head}" ++
                s"tail: ${pretty_path(tail)}" ++
                s"value: ${pretty_val(value)}"))
          }
      }
    }
  }

}

final case class EnvName(name: String, value: Val, rest: Env) extends Env
final case class EnvDecl(decl: Decl, rest: Env) extends Env
final case class EnvEmpty() extends Env

object Env {
  def apply(): Env = EnvEmpty()
}
