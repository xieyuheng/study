package xieyuheng.cicada

sealed trait Exp
final case class Var(name: String) extends Exp
final case class Type(level: Int) extends Exp
// final case class The(t: Exp, body: Exp) extends Exp
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
        ValClub(name, members, tel.put(arg))
      case ValMember(name, club_name, tel) =>
        ValMember(name, club_name, tel.put(arg))
      case ValRecord(name, super_names, tel) =>
        ValRecord(name, super_names, tel.put(arg))
      case neu: Neu =>
        NeuAp(neu, arg)
      case _ =>
        println(s"can not apply ${target}")
        throw new Exception()
    }
  }
}

object Choice {
  def ap(path: List[String], map: Map[String, Exp], env: Env): Val = {
    ???
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
      case _ =>
        println(s"can not apply dot type ${target}")
        throw new Exception()
    }
  }
}
