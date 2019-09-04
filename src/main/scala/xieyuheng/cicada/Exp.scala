package xieyuheng.cicada

sealed trait Exp
final case class Var(name: String) extends Exp
final case class Type() extends Exp
final case class The(t: Exp) extends Exp
final case class Case(target: Exp, map: MultiMap[String, Exp]) extends Exp
final case class Dot(target: Exp, fieldName: String) extends Exp
final case class Pi(args: MultiMap[String, Exp], ret: Exp) extends Exp
final case class Fn(args: MultiMap[String, Exp], ret: Exp, body: Exp) extends Exp
final case class Ap(target: Exp, args: MultiMap[String, Exp]) extends Exp

// def identifier = ???

// def exp: Rule = Rule(
//   "exp", Map(
//     "var" -> Seq(identifier),
//     "type" -> Seq("type"),
//     "case" -> Seq(exp, "case", "{", list(case_clause), "}"),
//     "dot" -> Seq(exp, ".", identifier),
//     "pi" -> Seq("pi", "(", list(arg), ")", ":", exp),
//     "fn" -> Seq("fn", "(", list(arg), ")", ":", exp, "=", exp),
//     "ap" -> Seq(exp, "(", list(arg), ")"),
//   ))

// def case_clause = Rule(
//   "case_clause", Map(
//     "clause" -> Seq(identifier, "=", ">", exp),
//   ))

// def arg = Rule(
//   "arg", Map(
//     "value" -> Seq(identifier, "=", exp),
//     "type" ->  Seq(identifier, ":", exp),
//     "value_comma" -> Seq(identifier, "=", exp, ","),
//     "type_comma" ->  Seq(identifier, ":", exp, ","),
//   ))
