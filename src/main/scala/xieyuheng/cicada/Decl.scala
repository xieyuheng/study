package xieyuheng.cicada

import scala.collection.immutable.ListMap

sealed trait Decl
final case class DefineValue(name: String, value: Value) extends Decl
final case class DefineClass(name: String, map: ListMap[String, Exp]) extends Decl
final case class DefineUnion(name: String, map: ListMap[String, Exp], subNames: List[String]) extends Decl
