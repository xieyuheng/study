package xieyuheng.minitt

sealed trait Value
final case class NeutralValue(neutral: Neutral) extends Value
final case class FnValue(fnclo: FnClosure) extends Value
final case class PiValue(t: Value, fnclo: FnClosure) extends Value
final case class SigmaValue(t: Value, fnclo: FnClosure) extends Value
final case object UValue extends Value
final case class ConsValue(car: Value, cdr: Value) extends Value
final case object SoleValue extends Value
final case object TrivialValue extends Value
final case class DataValue(tag: String, body: Value) extends Value
final case class SumValue(chclo: ChoiceClosure) extends Value
final case class ChoiceValue(chclo: ChoiceClosure) extends Value

case class FnClosure(pattern: Pattern, body: Exp, env: Env)
case class ChoiceClosure(choices: Map[String, Exp], env: Env)

sealed trait Neutral
final case class VarNeutral(name: String) extends Neutral
final case class ApNeutral(target: Neutral, arg: Value) extends Neutral
final case class CarNeutral(target: Neutral) extends Neutral
final case class CdrNeutral(target: Neutral) extends Neutral
final case class ChoiceNeutral(target: Neutral, chclo: ChoiceClosure) extends Neutral
