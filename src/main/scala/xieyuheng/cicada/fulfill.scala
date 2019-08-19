package xieyuheng.cicada

object fulfill {
  def apply(src: Value, tar: Value, bind: Bind): Either[ErrorMsg, Bind] = {
    (util.walk(src, bind), util.walk(tar, bind)) match {
      case (src, tar) if {
        src == tar
      } => {
        Right(bind)
      }

      case (value, t: LogicVar) => {
        Right(bind + (t.id -> value))
      }

      case (fn: FnValue, pi: PiValue) => {
        for {
          /** contravariant at args */
          bind1 <- forMap(pi.args, fn.args, bind)
          bind2 <- fulfill(fn.ret, pi.ret, bind1)
        } yield bind2 + (pi.id -> fn)
      }

      case (record: RecordValue, union: UnionValue) if {
        union.subNames.contains(record.name)
      } => {
        for {
          bind1 <- forBind(bind, record.bind)
          bind2 <- forBind(bind1, union.bind)
          bind3 <- forMap(record.map, union.map, bind2)
        } yield bind3 + (union.id -> record)
      }

      case (src: UnionValue, tar: UnionValue) if {
        src.name == tar.name
      } => {
        for {
          bind1 <- forBind(bind, src.bind)
          bind2 <- forBind(bind1, tar.bind)
          result <- forMap(src.map, tar.map, bind2)
        } yield result
      }

      case (src: RecordValue, tar: RecordValue) if {
        src.name == tar.name
      } => {
        for {
          bind1 <- forBind(bind, src.bind)
          bind2 <- forBind(bind1, tar.bind)
          result <- forMap(src.map, tar.map, bind2)
        } yield result
      }

      case (src: PiValue, tar: PiValue) => {
        for {
          bind1 <- forMap(tar.args, src.args, bind)
          bind2 <- fulfill(src.ret, tar.ret, bind1)
        } yield bind2
      }

      case _ => {
        Left(ErrorMsg(s"fail to fulfill src: ${src} into tar: ${tar}"))
      }
    }
  }

  def forMap(
    srcMap: MultiMap[String, Value],
    tarMap: MultiMap[String, Value],
    bind: Bind,
  ): Either[ErrorMsg, Bind] = {
    val initResult: Either[ErrorMsg, Bind] =
      Right(Bind())

    def updateBind(
      bind: Bind,
      name: String,
      tarValue: Value,
    ): Either[ErrorMsg, Bind] = {
      srcMap.get(name) match {
        case Some(srcValue) =>
          fulfill(srcValue, tarValue, bind)
        case None =>
          Left(ErrorMsg(s"srcMap does not have name: ${name}, tarValue: ${tarValue}"))
      }
    }

    tarMap.entries.foldLeft(initResult) { case (result, (name, tarValue)) =>
      result.flatMap { bind => updateBind(bind, name, tarValue) }
    }
  }

  def forBind(
    bind1: Bind,
    bind2: Bind,
  ): Either[ErrorMsg, Bind] = {
    val initBind = bind1 ++ bind2.filterNot { case (id, _value) =>
      bind1.keys.toSet.contains(id) }

    val initResult: Either[ErrorMsg, Bind] =
      Right(initBind)

    def updateBind(bind: Bind, id: Id, v: Value): Either[ErrorMsg, Bind] = {
      bind.get(id) match {
        case Some(v1) =>
          fulfill(v, v1, bind)
        case None =>
          Left(ErrorMsg(s"forBind internal error, bind1: ${bind1}, bind2: ${bind2}"))
      }
    }

    bind2
      .filter { case (id, _value) => bind1.keys.toSet.contains(id) }
      .foldLeft(initResult) { case (result, (id, v)) =>
        result.flatMap { bind => updateBind(bind, id, v) }
      }
  }
}
