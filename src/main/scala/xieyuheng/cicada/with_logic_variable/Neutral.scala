package xieyuheng.cicada.with_logic_variable

import scala.collection.immutable.ListMap

sealed trait Neu
final case class VarNeu(name: String) extends Neu
final case class ChoiceNeu(target: Neu, map: ListMap[String, Val]) extends Neu
final case class DotNeu(target: Neu, fieldName: String) extends Neu
final case class ApNeu(target: Neu, args: ListMap[String, Val]) extends Neu
