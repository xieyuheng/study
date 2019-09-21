package xieyuheng.cicada

sealed trait Val
final case class ValType(level: Int) extends Val
// final case class ValThe(t: Val, body: Val) extends Val
final case class ValPi(arg_name: String, arg_t: Val, dep_t: Clo) extends Val
final case class ValFn(arg_name: String, arg_t: Val, body: Clo) extends Val
final case class ValClub(name: String, members: List[Member], tel: Telescope) extends Val
final case class ValMember(name: String, club_name: String, tel: Telescope) extends Val
final case class ValRecord(name: String, super_names: List[String], tel: Telescope) extends Val

case class Clo(arg_name: String, body: Exp, env: Env) {
  def apply(arg: Val): Val = {
    eval(body, env.ext(arg_name, arg))
  }
}

object Telescope {
    def from_decls(decls: List[Decl], env: Env): Telescope = {
    // TODO
    Telescope(???, env)
  }
}

case class Telescope(
  fields: List[(String, Exp, Option[Exp], Option[Val], Option[Val])],
  env: Env,
) {

  def put(arg: Val): Telescope = {
    val i = fields.indexWhere {
      case (_, _, _, _, None) => true
      case (_, _, _, _, Some(vv)) => false
    }

    if (i == -1) {
      println(s"the telescope is full, fail to put: ${arg}")
      throw new Exception()
    } else {
      val (k, te, mve, _, _) = fields(i)
      val new_fields = util.list_replace(fields, i,
        (k, te, mve, Some(eval(te, env)), Some(arg)))
      Telescope(new_fields, env.ext(k, arg))
        .self_put()
    }
  }

  def self_put(): Telescope = {
    val i = fields.indexWhere {
      case (_, _, _, _, None) => true
      case (_, _, _, _, Some(vv)) => false
    }

    if (i == -1) {
      this
    } else {
      fields(i) match {
        case (k, te, Some(ve), _, _) =>
          val arg = eval(ve, env)
          val new_fields = util.list_replace(fields, i,
            (k, te, Some(ve), Some(eval(te, env)), Some(arg)))
          Telescope(new_fields, env.ext(k, arg))
        case _ => this
      }
    }
  }

  def dot(field_name: String): Val = {
    fields.find { case (k, _, _, _, _) => k == field_name } match {
      case Some((k, _, _, _, Some(vv))) => vv
      case _ =>
        println(s"can not find field_name: ${field_name}")
        throw new Exception()
    }
  }

  def dot_type(field_name: String): Val = {
    fields.find { case (k, _, _, _, _) => k == field_name } match {
      case Some((k, _, _, Some(tv), _)) => tv
      case _ =>
        println(s"can not find field_name: ${field_name}")
        throw new Exception()
    }
  }
}

sealed trait Neu extends Val
final case class NeuVar(name: String) extends Neu
final case class NeuAp(target: Neu, arg: Val) extends Neu
// NOTE do not store env in NeuChoice
// 1. deep_ext env ?
// 2. env can use path as key ?
final case class NeuChoice(target: Neu, map: Map[String, Exp], env: Env) extends Neu
final case class NeuDot(target: Neu, field_name: String) extends Neu
final case class NeuDotType(target: Neu, field_name: String) extends Neu
