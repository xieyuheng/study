package xieyuheng.cicada

object exe {
  def apply(
    target: Value,
    args: MultiMap[String, Value],
  ): Either[ErrorMsg, Value] = {
    target match {
      case t: LogicVar =>
        Left(ErrorMsg(s"can not apply a LogicVar: ${t}"))
      case union: UnionValue =>
        fulfill.forMap(args, union.map, union.bind).flatMap { newBind =>
          Right(union.copy(bind = newBind)) }
      case record: RecordValue =>
        fulfill.forMap(args, record.map, record.bind).flatMap { newBind =>
          Right(record.copy(bind = newBind)) }
      case pi: PiValue =>
        Left(ErrorMsg(s"can not apply a PiValue: ${pi}"))
      case fn: FnValue =>
        ???
      case neu: NeutralValue =>
        Right(NeutralValue(ApNeutral(neu.neutral, args)))
    }
  }
}
