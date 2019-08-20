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
        for {
          newBind <- unify.forMap(args, union.map, union.bind)
        } yield union.copy(bind = newBind)
      case record: RecordValue =>
        for {
          newBind <- unify.forMap(args, record.map, record.bind)
        } yield record.copy(bind = newBind)
      case pi: PiValue =>
        Left(ErrorMsg(s"can not apply a PiValue: ${pi}"))
      case fn: FnValue =>
        ???
      case neu: NeutralValue =>
        Right(NeutralValue(ApNeutral(neu.neutral, args)))
    }
  }
}
