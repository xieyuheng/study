package xieyuheng.cicada

import pretty._

sealed trait Exp
final case class Var(name: String) extends Exp
final case class Type(level: Int) extends Exp
final case class Pi(arg_name: String, arg_t: Exp, dep_t: Exp) extends Exp
final case class Fn(arg_name: String, arg_t: Exp, body: Exp) extends Exp
final case class Ap(target: Exp, arg: Exp) extends Exp
final case class Choice(path: List[String], map: Map[String, Exp]) extends Exp
final case class Dot(target: Exp, field_name: String) extends Exp
final case class DotType(target: Exp, field_name: String) extends Exp
final case class Let(decl: Decl, body: Exp) extends Exp

object Ap {
  def ap(target: Val, arg: Val): Val = {
    target match {
      case ValFn(arg_name: String, arg_t: Val, body: Clo) =>
        body(arg)
      case ValClub(name, members, tel) =>
        ValClub(name, members, util.unwrap(tel.put(arg)))
      case ValMember(name, club_name, tel) =>
        ValMember(name, club_name, util.unwrap(tel.put(arg)))
      case ValRecord(name, super_names, tel) =>
        ValRecord(name, super_names, util.unwrap(tel.put(arg)))
      case neu: Neu =>
        NeuAp(neu, arg)
      case _ =>
        println(s"can not apply ${target}")
        throw new Exception()
    }
  }
}

object Choice {

  def path_as_exp(path: List[String]): Exp = {
    assert(path.length > 0)
    val init: Exp = Var(path.head)
    path.tail.foldLeft(init) { case (exp, field_name) => Dot(exp, field_name) }
  }

  def ap(path: List[String], map: Map[String, Exp], env: Env): Val = {
    val exp = Choice.path_as_exp(path)
    val value = eval(exp, env)
    // TODO handle subtype relation in choice
    value match {
      case ValClub(name: String, members: List[Member], tel: Telescope) =>
        map.get(name) match {
          case Some(body) => eval(body, env)
          case None =>
            println(s"choice mismatch: ${pretty_val(value)}")
            println(s"${pretty_exp_case(map)}")
            throw new Exception()
        }
      case ValMember(name: String, club_name: String, tel: Telescope) =>
        map.get(name) match {
          case Some(body) => eval(body, env)
          case None =>
            println(s"choice mismatch: ${pretty_val(value)}")
            println(s"${pretty_exp_case(map)}")
            throw new Exception()
        }
      case ValRecord(name: String, super_names: List[String], tel: Telescope) =>
        map.get(name) match {
          case Some(body) => eval(body, env)
          case None =>
            println(s"choice mismatch: ${pretty_val(value)}")
            println(s"${pretty_exp_case(map)}")
            throw new Exception()
        }
      case neu: Neu =>
        NeuChoice(neu, path, map, env)
      case _ =>
        println(s"choice mismatch: ${pretty_val(value)}")
        println(s"${pretty_exp_case(map)}")
        throw new Exception()
    }
  }
}

object Dot {
  def ap(target: Val, field_name: String): Val = {
    target match {
      case ValClub(name, members, tel) =>
        tel.dot(field_name)
      case ValMember(name, club_name, tel) =>
        tel.dot(field_name)
      case ValRecord(name, super_names, tel) =>
        tel.dot(field_name)
      case neu: Neu =>
        NeuDot(neu, field_name)
      case _ =>
        println(s"can not apply dot ${target}")
        throw new Exception()
    }
  }
}

object DotType {
  def ap(target: Val, field_name: String): Val = {
    target match {
      case ValClub(name, members, tel) =>
        tel.dot_type(field_name)
      case ValMember(name, club_name, tel) =>
        tel.dot_type(field_name)
      case ValRecord(name, super_names, tel) =>
        tel.dot_type(field_name)
      case neu: Neu =>
        NeuDotType(neu, field_name)
      case _ =>
        println(s"can not apply dot type ${target}")
        throw new Exception()
    }
  }
}
