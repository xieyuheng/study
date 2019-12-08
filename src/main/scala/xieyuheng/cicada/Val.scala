package xieyuheng.cicada

import collection.immutable.ListMap

sealed trait Val
final case class ValType() extends Val
final case class ValPi(arg_name: String, arg_type: Exp, ret_type: Exp, env: Env) extends Val
final case class ValFn(arg_name: String, arg_type: Exp, body: Exp, env: Env) extends Val
final case class ValClass(type_map: ListMap[String, Exp], env: Env) extends Val
final case class ValObject(val_map: ListMap[String, Val]) extends Val

sealed trait Neu extends Val
final case class NeuVar(name: String) extends Neu
final case class NeuAp(target: Neu, arg: Val) extends Neu
final case class NeuDot(target: Neu, field: String) extends Neu
