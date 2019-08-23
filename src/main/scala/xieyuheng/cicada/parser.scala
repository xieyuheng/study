package xieyuheng.cicada

import fastparse._, NoWhitespace._

object parser {
  // def pVar[_: P]: P[Var] = P(
  //   CharIn("0-9").!.map(_.toInt)
  // )

  // final case class Var(name: String) extends Exp

  // final case class Type() extends Exp
  def pType[_: P]: P[Type] = P(
    "type_t"
  ).map(_ => Type())

  // final case class The(t: Exp) extends Exp
  // final case class Case(target: Exp, map: MultiMap[String, Exp]) extends Exp
  // final case class Field(target: Exp, fieldName: String) extends Exp
  // final case class Pi(args: MultiMap[String, Exp], ret: Exp) extends Exp
  // final case class Fn(args: MultiMap[String, Exp], ret: Exp, body: Exp) extends Exp
  // final case class Ap(target: Exp, args: MultiMap[String, Exp]) extends Exp
}

object parserTest extends App {
  println(parse("type_t", parser.pType(_)))
}
