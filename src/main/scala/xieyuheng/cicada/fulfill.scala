package xieyuheng.cicada

import join._
import pretty._
import readback._

object fulfill {

  def fulfill_val(x: Val, y: Val): Either[Err, Unit] = {
    (x, y) match {
      case (x: ValPi, y: ValPi) =>
        for {
          _ <- fulfill_val(y.arg_t, x.arg_t)
          _ <- fulfill_clo(x.dep_t, y.dep_t)
        } yield ()
      case (x: ValFn, y: ValFn) =>
        for {
          _ <- fulfill_val(y.arg_t, x.arg_t)
          _ = {
            println("------")
          }
          _ <- fulfill_clo(x.body, y.body)
        } yield ()
      case (x: ValFn, y: ValPi) =>
        for {
          _ <- fulfill_val(y.arg_t, x.arg_t)
          _ <- fulfill_clo(x.body, y.dep_t)
        } yield ()
      case (x: ValClub, y: ValClub) =>
        if (x.name == y.name) {
          fulfill_tel(x.tel, y.tel)
        } else {
          Left(Err(
            s"[fulfill_val fail]\n" ++
              s"x: ${pretty_val(x)}\n" ++
              s"y: ${pretty_val(y)}\n"))
        }
      case (x: ValMember, y: ValMember) =>
        if (x.name == y.name) {
          fulfill_tel(x.tel, y.tel)
        } else {
          Left(Err(
            s"[fulfill_val fail]\n" ++
              s"x: ${pretty_val(x)}\n" ++
              s"y: ${pretty_val(y)}\n"))
        }
      case (x: ValMember, y: ValClub) =>
        if (x.club_name == y.name) {
          fulfill_tel(x.tel, y.tel)
        } else {
          Left(Err(
            s"[fulfill_val fail]\n" ++
              s"x: ${pretty_val(x)}\n" ++
              s"y: ${pretty_val(y)}\n"))
        }
      case (x: ValRecord, y: ValRecord) =>
        if (x.name == y.name) {
          fulfill_tel(x.tel, y.tel)
        } else {
          // TODO handle extends
          Left(Err(
            s"[fulfill_val fail]\n" ++
              s"x: ${pretty_val(x)}\n" ++
              s"y: ${pretty_val(y)}\n"))
        }
      case (x, ValType(level)) =>
        fulfill_type(x, level)
      case (x: ValClub, y: ValPi) =>
        fields_find_first_none(x.tel.fields) match {
          case Some((k, te, mve, Some(tv), None)) =>
            for {
              _ <- fulfill_val(y.arg_t, tv)
              new_tel <- x.tel.put(gen_neu_val(k, tv, None))
            } yield fulfill_val(x.copy(tel = new_tel), y.dep_t.force())
          case Some((k, te, mve, None, None)) =>
            val tv = eval(te, x.tel.env)
            for {
              _ <- fulfill_val(y.arg_t, tv)
              new_tel <- x.tel.put(gen_neu_val(k, tv, None))
            } yield fulfill_val(x.copy(tel = new_tel), y.dep_t.force())
          case _ =>
            Left(Err(
              s"[fulfill_val fail]\n" ++
                s"x: ${pretty_val(x)}\n" ++
                s"y: ${pretty_val(y)}\n"))
        }
      case (x: ValMember, y: ValPi) =>
        fields_find_first_none(x.tel.fields) match {
          case Some((k, te, mve, Some(tv), None)) =>
            for {
              _ <- fulfill_val(y.arg_t, tv)
              new_tel <- x.tel.put(gen_neu_val(k, tv, None))
            } yield fulfill_val(x.copy(tel = new_tel), y.dep_t.force())
          case Some((k, te, mve, None, None)) =>
            val tv = eval(te, x.tel.env)
            for {
              _ <- fulfill_val(y.arg_t, tv)
              new_tel <- x.tel.put(gen_neu_val(k, tv, None))
            } yield fulfill_val(x.copy(tel = new_tel), y.dep_t.force())
          case _ =>
            Left(Err(
              s"[fulfill_val fail]\n" ++
                s"x: ${pretty_val(x)}\n" ++
                s"y: ${pretty_val(y)}\n"))
        }
      case (x: ValRecord, y: ValPi) =>
        fields_find_first_none(x.tel.fields) match {
          case Some((k, te, mve, Some(tv), None)) =>
            for {
              _ <- fulfill_val(y.arg_t, tv)
              new_tel <- x.tel.put(gen_neu_val(k, tv, None))
            } yield fulfill_val(x.copy(tel = new_tel), y.dep_t.force())
          case Some((k, te, mve, None, None)) =>
            val tv = eval(te, x.tel.env)
            for {
              _ <- fulfill_val(y.arg_t, tv)
              new_tel <- x.tel.put(gen_neu_val(k, tv, None))
            } yield fulfill_val(x.copy(tel = new_tel), y.dep_t.force())
          case _ =>
            Left(Err(
              s"[fulfill_val fail]\n" ++
                s"x: ${pretty_val(x)}\n" ++
                s"y: ${pretty_val(y)}\n"))
        }
      case (x: Neu, y) =>
        val list = infer_neu(x).map {
          case result =>
            result.flatMap { case x => fulfill_val(x, y) } }
        first_err(list)
      case (x, y: Neu) =>
        val list = infer_neu(y).map {
          case result =>
            result.flatMap { case y => fulfill_val(x, y) } }
        first_err(list)
      case _ =>
        Left(Err(
          s"[fulfill_val fail]\n" ++
            s"x: ${pretty_val(x)}\n" ++
            s"y: ${pretty_val(y)}\n"))
    }
  }

  def fulfill_clo(x: Clo, y: Clo): Either[Err, Unit] = {
    fulfill_val(x.force(), y.force())
  }

  def fulfill_tel(x: Tel, y: Tel): Either[Err, Unit] = {
    val x_forced = x.force()
    val y_forced = y.force()

    val list = y_forced.fields.map {
      case (k, te, mve, Some(tv), Some(vv)) =>
        x_forced.fields.find { case (k2, _, _, _, _) => k == k2 } match {
          case Some((k2, te2, mve2, Some(tv2), Some(vv2))) =>
            fulfill_val(tv2, tv).flatMap { _ => fulfill_val(vv2, vv) }
          case Some(_) =>
            println(s"[internal error]")
            println(s"tel.force is not effective")
            throw new Exception()
          case None =>
            Left(Err(
              s"[fulfill_tel fail]\n" ++
                s"x: ${pretty_tel(x)}\n" ++
                s"y: ${pretty_tel(y)}\n"))
        }
      case _ =>
        println(s"[internal error]")
        println(s"tel.force is not effective")
        throw new Exception()
    }

    first_err(list)
  }

  def first_err(list: List[Either[Err, Unit]]): Either[Err, Unit] = {
    list.find {
      case Right(_) => false
      case Left(_) => true
    } match {
      case Some(left) => left
      case None => Right(())
    }
  }

  def fields_find_first_none(
    fields: List[(String, Exp, Option[Exp], Option[Val], Option[Val])]
  ): Option[(String, Exp, Option[Exp], Option[Val], Option[Val])] = {
    fields.find {
      case (k, te, mve, mtv, None) => true
      case (k, te, mve, mtv, Some(vv)) => false
    }
  }

  def infer_neu(neu: Neu): List[Either[Err, Val]] = {
    neu match {
      case NeuVar(name: String, arg_t: Val, aka) =>
        List(Right(arg_t))
      case NeuAp(target: Neu, arg: Val) =>
        infer_neu(target).flatMap {
          case Left(err) =>
            List(Left(err))
          case Right(ValPi(arg_name, arg_t, dep_t: Clo)) =>
            List(fulfill_val(arg, arg_t).flatMap { _ => Right(dep_t(arg)) })
          case Right(ValFn(arg_name, arg_t, dep_t: Clo, body: Clo)) =>
            List(fulfill_val(arg, arg_t).flatMap { _ => Right(dep_t(arg)) })
          case Right(ValClub(name, members, tel)) =>
            List(tel.put(arg).flatMap { case new_tel =>
              Right(ValClub(name, members, new_tel)) })
          case Right(ValMember(name, club_name, tel)) =>
            List(tel.put(arg).flatMap { case new_tel =>
              Right(ValMember(name, club_name, new_tel)) })
          case Right(ValRecord(name, super_names, tel)) =>
            List(tel.put(arg).flatMap { case new_tel =>
              Right(ValRecord(name, super_names, new_tel)) })
          case Right(_) =>
            List(Left(Err(
              s"[infer_neu fail]\n" ++
                s"neu: ${pretty_val(neu)}\n")))
        }
      case NeuChoice(target, path, map, env) =>
        infer_neu(target).flatMap {
          case Right(t) =>
            map.toList.map { case (choice_name, body) =>
              refine_choice(t, choice_name, body, path, env).map {
                case value =>
                  println(s"target: ${pretty_neu(target)}")
                  println(s"path: ${pretty_path(path)}")
                  println(s"path_type: ${pretty_val(t)}")
                  println(s"choice_name: ${choice_name}")
                  println(s"body: ${pretty_exp(body)}")
                  println(s"refined_body: ${pretty_val(value)}")
                  println() }
              refine_choice(t, choice_name, body, path, env) }
          case Left(err) => List(Left(err)) }
      case NeuDot(target: Neu, field_name: String) =>
        infer_neu(target).flatMap {
          case Left(err) =>
            List(Left(err))
          case Right(ValClub(name: String, members: List[Member], tel: Tel)) =>
            List(infer_tel_dot(tel: Tel, field_name: String))
          case Right(ValMember(name: String, club_name: String, tel: Tel)) =>
            List(infer_tel_dot(tel: Tel, field_name: String))
          case Right(ValRecord(name: String, super_names: List[String], tel: Tel)) =>
            List(infer_tel_dot(tel: Tel, field_name: String))
          case Right(_) =>
            List(Left(Err(
              s"[infer_neu fail]\n" ++
                s"neu: ${pretty_val(neu)}\n")))
        }
      case NeuDotType(target: Neu, field_name: String) =>
        // TODO
        ???
    }
  }

  def infer_tel_dot(tel: Tel, field_name: String): Either[Err, Val] = {
    val tel_forced = tel.force()

    tel_forced.fields.find {
      case (k, _, _, _, _) =>
        k == field_name
    } match {
      case Some((_, _, _, Some(tv), _)) =>
        Right(tv)
      case Some((_, _, _, None, _)) =>
        println(s"[internal error]")
        println(s"tel.force is not effective")
        throw new Exception()
      case None =>
        Left(Err(
          s"[infer_tel_dot fail]\n" ++
            s"tel: ${pretty_tel(tel)}\n" ++
            s"field_name: ${field_name}\n"))
    }
  }

  def refine_choice(
    t: Val,
    choice_name: String,
    body: Exp,
    path: List[String],
    env: Env,
  ): Either[Err, Val] = {
    env.lookup_val(choice_name) match {
      case Some(value) =>
        for {
          refined_val <- join_val(t, value)
          refined_env <- env.ext_by_path(path, refined_val)
          // _ = {
          //   println(s"- t: ${pretty_val(t)}")
          //   println(s"- choice_name: ${choice_name}")
          //   println(s"- refined_val: ${pretty_val(refined_val)}")
          //   println(s"- refined_path_val: ${pretty_val(eval(Choice.path_as_exp(path), refined_env))}")
          // }
        } yield infer(body, refined_env)
      case None =>
        Left(Err(
          s"[refine_choice fail]\n" ++
            s"t: ${pretty_val(t)}\n" ++
            s"path: ${pretty_path(path)}\n" ++
            s"choice_name: ${choice_name}\n" ++
            s"body: ${pretty_exp(body)}\n"))
    }
  }

  def fulfill_type(x: Val, level: Int): Either[Err, Unit] = {
    x match {
      case neu: Neu =>
        first_err(infer_neu(neu).map {
          // NOTE maybe need to dec level here
          case Right(t) => fulfill_type(t, level)
          case Left(err) => Left(err)
        })
      case x: ValType =>
        if (x.level < level) {
          Right(())
        } else {
          Left(Err(
            s"[fulfill_type fail]\n" ++
              s"x: ${pretty_val(x)}\n" ++
              s"level: ${level}\n"))
        }
      case x: ValPi =>
        for {
          _ <- fulfill_type(x.arg_t, level)
          _ <- fulfill_type(x.dep_t.force(), level)
        } yield ()
      case x: ValFn =>
        for {
          _ <- fulfill_type(x.arg_t, level)
          _ <- fulfill_type(x.body.force(), level)
        } yield ()
      case x: ValClub =>
        fulfill_tel_type(x.tel, level)
      case x: ValMember =>
        fulfill_tel_type(x.tel, level)
      case x: ValRecord =>
        fulfill_tel_type(x.tel, level)
      case _ =>
        Left(Err(
          s"[fulfill_type fail]\n" ++
            s"x: ${pretty_val(x)}\n" ++
            s"level: ${level}\n"))
    }
  }

  def fulfill_tel_type(x: Tel, level: Int): Either[Err, Unit] = {
    val list = x.force().fields.map {
      case (_, _, _, Some(tv), _) => fulfill_type(tv, level)
      case (_, _, _, None, _) =>
        println(s"[internal error]")
        println(s"tel.force is not effective")
        throw new Exception()
    }
    first_err(list)
  }

}
