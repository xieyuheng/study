package xieyuheng.cicada

import pretty._
import readback._

case class Tel(
  fields: List[(String, Exp, Option[Exp], Option[Val], Option[Val])],
  env: Env,
) {

  def put(arg: Val): Either[Err, Tel] = {
    val tel = this.self_put()

    val i = tel.fields.indexWhere {
      case (_, _, _, _, None) => true
      case (_, _, _, _, Some(_)) => false
    }

    if (i == -1) {
      Left(Err(
        s"[tel.put]\n" ++
          s"the telescope is full\n" ++
          s"fail to put arg: ${pretty_val(arg)}\n" ++
          s"tel: ${pretty_tel(tel)}\n"))
    } else {
      val (k, te, mve, _, _) = tel.fields(i)
      val new_fields = util.list_replace(tel.fields, i,
        (k, te, mve, Some(eval(te, tel.env)), Some(arg)))
      val new_tel = Tel(new_fields, tel.env.ext_val(k, arg))
      Right(new_tel.self_put())
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

  def force(): Tel = {
    //println(s"[tel.force] begin")
    //println(s"tel: ${pretty_tel(this)}")
    val forced = fields.foldLeft(this) {
      case (tel, (k, te, mve, Some(tv), None)) =>
        // println(s"(tel, (k, te, mve, Some(tv), None))")
        // println(s"--- ${pretty_tel(util.result_unwrap(tel.put(gen_neu_val(k, tv, None))))}")
        util.result_unwrap(tel.put(gen_neu_val(k, tv, None)))
      case (tel, (k, te, mve, None, None)) =>
        // println(s"(tel, (k, te, mve, None, None))")
        // println(s"k: ${k}")
        // println(s"te: ${pretty_exp(te)}")
        // println(s"--- ${pretty_tel(util.result_unwrap(tel.put(gen_neu_val(k, eval(te, tel.env), None))))}")
        util.result_unwrap(tel.put(gen_neu_val(k, eval(te, tel.env), None)))
      case (tel, (k, te, mve, mtv, Some(vv))) =>
        // println(s"(tel, (k, te, mve, mtv, Some(vv)))")
        // println(s"k: ${k}")
        // println(s"te: ${pretty_exp(te)}")
        // println(s"mve: ${mve}")
        // println(s"mtv: ${mtv}")
        // println(s"vv: ${pretty_val(vv)}")
        // TODO in some case vv is put to the tel, but the tel.env is not extended
        //   should be enough to return `tel`
        //   but I did an ad hoc fix to extend env
        // tel
        Tel(tel.fields, tel.env.ext_val(k, vv))

    }
    // println(s"[tel.force] end")
    forced
  }

  def dot(field_name: String): Val = {
    fields.find { case (k, _, _, _, _) => k == field_name } match {
      case Some((k, te, mve, mtv, Some(vv))) =>
        vv
      case Some((k, te, mve, Some(tv), None)) =>
        // NOTE is it enough to generate a NeuVar here ?
        val arg_t = tv
        NeuVar("_", arg_t, None)
      case Some((k, te, mve, None, None)) =>
        // NOTE is it enough to generate a NeuVar here ?
        val arg_t = eval(te, this.env)
        NeuVar("_", arg_t, None)
      case _ =>
        println(s"[dot fail]")
        println(s"can not find field_name: ${field_name}")
        println(s"tel: ${pretty_tel(this)}")
        throw new Exception()
    }
  }

  def dot_type(field_name: String): Val = {
    fields.find { case (k, _, _, _, _) => k == field_name } match {
      case Some((k, te, mve, Some(tv), mvv)) =>
        tv
      case Some((k, te, mve, None, mvv)) =>
        eval(te, this.env)
      case _ =>
        println(s"[dot_type fail]")
        println(s"can not find field_name: ${field_name}")
        println(s"tel: ${pretty_tel(this)}")
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
        val (pi, fn) = args.foldRight(dep_t, body) { case ((arg_name, arg_t), (dep_t, body)) =>
          val pi = Pi(arg_name, arg_t, dep_t)
          val fn = Fn(arg_name, arg_t, dep_t, body)
          (pi, fn) }
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
