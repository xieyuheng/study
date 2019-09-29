package xieyuheng.cicada

import join._
import pretty._
import fulfill._

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
            println(s"[infer fail]")
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
        ValPi(arg_name, arg_t_val, dep_t_clo)
        // ValFn(arg_name, arg_t_val, dep_t_clo, body_clo)
      case Ap(target, arg) =>
        // println(s"[infer] on Ap")
        // println(s"target: ${pretty_exp(target)}")
        // println(s"arg: ${pretty_exp(arg)}")
        // println(s"infer(target, env): ${pretty_val(infer(target, env))}")
        // println(s"infer(arg, env): ${pretty_val(infer(arg, env))}")
        ap_exe(infer(target, env), infer(arg, env))
      case Choice(path, map: Map[String, Exp]) =>
        choice_exe(path, map, env)
      case Dot(target, field_name) =>
        // println(s"[infer] on Dot")
        // println(s"target: ${pretty_exp(target)}")
        // println(s"field_name: ${field_name}")
        // println(s"infer(target, env): ${pretty_val(infer(target, env))}")
        // println(s"dot_type_exe(infer(target, env), field_name): ${pretty_val(dot_type_exe(infer(target, env), field_name))}")
        dot_type_exe(infer(target, env), field_name)
      case DotType(target, field_name) =>
        dot_type_exe(infer(target, env), field_name)
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


  def ap_exe(target: Val, arg: Val): Val = {
    target match {
      case ValPi(arg_name: String, arg_t: Val, dep_t: Clo) =>
        dep_t(arg)
      case ValFn(arg_name: String, arg_t: Val, dep_t: Clo, body: Clo) =>
        // NOTE this is the different between `eval`
        dep_t(arg)
      case ValClub(name, members, tel) =>
        ValClub(name, members, util.result_unwrap(tel.put(arg)))
      case ValMember(name, club_name, tel) =>
        // println(s"[ap_exe]")
        // println(s"name: ${name}")
        // println(s"club_name: ${club_name}")
        // println(s"tel: ${pretty_tel(tel)}")
        ValMember(name, club_name, util.result_unwrap(tel.put(arg)))
      case ValRecord(name, super_names, tel) =>
        ValRecord(name, super_names, util.result_unwrap(tel.put(arg)))
      case neu: Neu =>
        NeuAp(neu, arg)
      case _ =>
        println(s"infer can not apply ${target}")
        throw new Exception()
    }
  }

  def path_as_exp(path: List[String]): Exp = {
    assert(path.length > 0)
    val init: Exp = Var(path.head)
    path.tail.foldLeft(init) { case (exp, field_name) => Dot(exp, field_name) }
  }

  def choice_exe(path: List[String], map: Map[String, Exp], env: Env): Val = {
    val exp = Exp.from_path(path)
    val value = infer(exp, env)
    // TODO handle subtype relation in choice
    value match {
      case ValClub(name: String, members: List[Member], tel: Tel) =>
        map.get(name) match {
          case Some(body) => infer(body, env)
          case None =>
            println(s"[infer choice_exe]")
            println(s"choice mismatch: ${pretty_val(value)}")
            println(s"${pretty_exp_case(map)}")
            throw new Exception()
        }
      case ValMember(name: String, club_name: String, tel: Tel) =>
        map.get(name) match {
          case Some(body) => infer(body, env)
          case None =>
            println(s"[infer choice_exe]")
            println(s"choice mismatch: ${pretty_val(value)}")
            println(s"${pretty_exp_case(map)}")
            throw new Exception()
        }
      case ValRecord(name: String, super_names: List[String], tel: Tel) =>
        map.get(name) match {
          case Some(body) => infer(body, env)
          case None =>
            println(s"[infer choice_exe]")
            println(s"choice mismatch: ${pretty_val(value)}")
            println(s"${pretty_exp_case(map)}")
            throw new Exception()
        }
      case neu: Neu =>
        NeuChoice(neu, path, map, env)
      case _ =>
        println(s"[infer choice_exe]")
        println(s"choice mismatch: ${pretty_val(value)}")
        println(s"${pretty_exp_case(map)}")
        throw new Exception()
    }
  }

  def dot_exe(target: Val, field_name: String): Val = {
    target match {
      case ValClub(name, members, tel) =>
        tel.dot_type(field_name)
      case ValMember(name, club_name, tel) =>
        tel.dot_type(field_name)
      case ValRecord(name, super_names, tel) =>
        tel.dot_type(field_name)
      case neu: Neu =>
        NeuDot(neu, field_name)
      case _ =>
        println(s"[infer fail]")
        println(s"can not apply dot ${target}")
        throw new Exception()
    }
  }

  def dot_type_exe(target: Val, field_name: String): Val = {
    target match {
      case ValClub(name, members, tel) =>
        tel.force().dot_type(field_name)
      case ValMember(name, club_name, tel) =>
        tel.force().dot_type(field_name)
      case ValRecord(name, super_names, tel) =>
        tel.force().dot_type(field_name)
      case neu: Neu =>
        NeuDotType(neu, field_name)
      case _ =>
        println(s"[infer fail]")
        println(s"can not apply dot type ${target}")
        throw new Exception()
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
            // List(Right(dep_t(arg)))
          case Right(ValFn(arg_name, arg_t, dep_t: Clo, body: Clo)) =>
            // List(fulfill_val(arg, arg_t).flatMap { _ => Right(dep_t(arg)) })
            List(fulfill_val(arg, arg_t).flatMap { _ => Right(body(arg)) })
            // List(Right(dep_t(arg)))
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
              // println(s"choice_name: ${choice_name}")
              // println(s"body: ${pretty_exp(body)}")
              // refine_choice(t, choice_name, body, path, env).map {
              //   case value =>
              //     println(s"target: ${pretty_neu(target)}")
              //     println(s"path: ${pretty_path(path)}")
              //     println(s"path_type: ${pretty_val(t)}")
              //     println(s"refined_body: ${pretty_val(value)}")
              //     println() }
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
          _ = {
            println(s"[refine_choice]")
            println(s"t: ${pretty_val(t)}")
            println(s"choice_name: ${choice_name}")
            println(s"refined_val: ${pretty_val(refined_val)}")
            println(s"refined_path_val: ${pretty_val(eval(Exp.from_path(path), refined_env))}")
            println(s"body: ${pretty_exp(body)}")
            println(s"infer(body, refined_env): ${pretty_val(infer(body, refined_env))}")
          }
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

  def infer_val(value: Val): List[Either[Err, Val]] = {
    value match {
      case _ => ???
    }
  }
}
