package xieyuheng.cicada

sealed trait Val
final case class ValType(level: Int) extends Val
final case class ValPi(arg_name: String, arg_t: Val, dep_t: Clo) extends Val
final case class ValFn(arg_name: String, arg_t: Val, body: Clo) extends Val
final case class ValClub(name: String, members: List[Member], tel: Tel) extends Val
final case class ValMember(name: String, club_name: String, tel: Tel) extends Val
final case class ValRecord(name: String, super_names: List[String], tel: Tel) extends Val

sealed trait Neu extends Val
final case class NeuVar(name: String, arg_t: Val, aka: Option[String]) extends Neu {
  val matters = (name, arg_t)

  override def equals(that: Any): Boolean = {
    that match {
      case that: NeuVar => this.matters == that.matters
      case _ => false
    }
  }

  override def hashCode = matters.hashCode
}
final case class NeuAp(target: Neu, arg: Val) extends Neu
// NOTE do not store env in NeuChoice
// 1. deep_ext env ?
// 2. env can use path as key ?
final case class NeuChoice(target: Neu, path: List[String], map: Map[String, Exp], env: Env) extends Neu
final case class NeuDot(target: Neu, field_name: String) extends Neu
final case class NeuDotType(target: Neu, field_name: String) extends Neu

case class Clo(arg_name: String, body: Exp, env: Env) {
  def apply(arg: Val): Val = {
    eval(body, env.ext_val(arg_name, arg))
  }
}

case class Tel(
  fields: List[(String, Exp, Option[Exp], Option[Val], Option[Val])],
  env: Env,
) {

  def put(arg: Val): Either[Err, Tel] = {
    val i = fields.indexWhere {
      case (_, _, _, _, None) => true
      case (_, _, _, _, Some(_)) => false
    }

    if (i == -1) {
      Left(Err(s"the telescope is full, fail to put: ${arg}"))
    } else {
      val (k, te, mve, _, _) = fields(i)
      val new_fields = util.list_replace(fields, i,
        (k, te, mve, Some(eval(te, env)), Some(arg)))
      Right(
        Tel(new_fields, env.ext_val(k, arg))
          .self_put())
    }
  }

  def self_put(): Tel = {
    val i = fields.indexWhere {
      case (_, _, _, _, None) => true
      case (_, _, _, _, Some(_)) => false
    }

    if (i == -1) {
      this
    } else {
      fields(i) match {
        case (k, te, Some(ve), _, _) =>
          val arg = eval(ve, env)
          val new_fields = util.list_replace(fields, i,
            (k, te, Some(ve), Some(eval(te, env)), Some(arg)))
          Tel(new_fields, env.ext_val(k, arg))
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

object Tel {

  def from_exp_fields(
    fields: List[(String, Exp, Option[Exp])],
    env: Env,
  ): Tel = {
    val val_fiedls = fields.map { case (k, te, mve) => (k, te, mve, None, None) }
    Tel(val_fiedls, env)
  }

  def from_decls(
    decls: List[Decl],
    env: Env,
  ): Tel = {
    var val_fiedls: List[(String, Exp, Option[Exp], Option[Val], Option[Val])] = List()

    decls.foreach {
      case DeclLet(name, t, body) =>
        val_fiedls = val_fiedls :+ ((name, t, Some(body), None, None))
      case DeclLetType(name, t) =>
        val_fiedls = val_fiedls :+ ((name, t, None, None, None))
      case DeclFn(name, args, dep_t, body) =>
        // NOTE using body as type here
        //   which might be wrong
        val pi = args.foldRight(dep_t) { case ((arg_name, arg_t), pi) => Pi(arg_name, arg_t, pi) }
        val fn = args.foldRight(body) { case ((arg_name, arg_t), fn) => Fn(arg_name, arg_t, fn) }
        val_fiedls = val_fiedls :+ ((name, pi, Some(fn), None, None))
      case DeclFnType(name, args, dep_t) =>
        val pi = args.foldRight(dep_t) { case ((arg_name, arg_t), pi) => Pi(arg_name, arg_t, pi) }
        val_fiedls = val_fiedls :+ ((name, pi, None, None, None))
      case DeclClub(name, members, fields) =>
        // TODO fix the level of type
        //   should depends on fields
        // TODO since we do not have exp for club and record
        //   we can only create the value at init time
        //   this is wrong, because they can depend on value of prev fields
        // NOTE
        //   I forget what I meant when I said the above sentence
        //   I can not see what is wrong now
        val club_val = ValClub(name, members, Tel.from_exp_fields(fields, env))
        val_fiedls = val_fiedls :+ ((name, Type(1), None, Some(ValType(1)), Some(club_val)))
      case DeclRecord(name, super_names, decls) =>
        val record_val = ValRecord(name, super_names, Tel.from_decls(decls, env))
        val_fiedls = val_fiedls :+ ((name, Type(1), None, Some(ValType(1)), Some(record_val)))
    }

    Tel(val_fiedls, env)
  }

}
