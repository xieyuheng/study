package xieyuheng.cicada

object unify {
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

      case (t: LogicVar, value) => {
        Right(bind + (t.id -> value))
      }

      case (fn: FnValue, pi: PiValue) => {
        for {
          /** contravariant at args */
          bind1 <- forMap(pi.args, fn.args, bind)
          bind2 <- unify(fn.ret, pi.ret, bind1)
        } yield bind2 + (pi.id -> fn)
      }

      case (pi: PiValue, fn: FnValue) => {
        for {
          /** contravariant at args */
          bind1 <- forMap(pi.args, fn.args, bind)
          bind2 <- unify(fn.ret, pi.ret, bind1)
        } yield bind2 + (pi.id -> fn)
      }

      case (record: RecordValue, union: UnionValue) if {
        union.subNames.contains(record.name)
      } => {
        for {
          bind1 <- forBind(record.bind, bind)
          bind2 <- forBind(union.bind, bind1)
          bind3 <- forMap(record.map, union.map, bind2)
        } yield bind3 + (union.id -> record)
      }

      case (union: UnionValue, record: RecordValue) if {
        union.subNames.contains(record.name)
      } => {
        for {
          bind1 <- forBind(record.bind, bind)
          bind2 <- forBind(union.bind, bind1)
          bind3 <- forMap(record.map, union.map, bind2)
        } yield bind3 + (union.id -> record)
      }

      case (src: UnionValue, tar: UnionValue) if {
        src.name == tar.name
      } => {
        for {
          bind1 <- forBind(src.bind, bind)
          bind2 <- forBind(tar.bind, bind1)
          result <- forMap(src.map, tar.map, bind2)
        } yield result
      }

      case (src: RecordValue, tar: RecordValue) if {
        src.name == tar.name
      } => {
        for {
          bind1 <- forBind(src.bind, bind)
          bind2 <- forBind(tar.bind, bind1)
          result <- forMap(src.map, tar.map, bind2)
        } yield result
      }

      case (src: PiValue, tar: PiValue) => {
        for {
          bind1 <- forMap(tar.args, src.args, bind)
          bind2 <- unify(src.ret, tar.ret, bind1)
        } yield bind2
      }

      case _ => {
        Left(ErrorMsg(
          "fail to unify\n" ++
            s"src: ${util.walk(src, bind)}\n" ++
            s"tar: ${util.walk(tar, bind)}\n" ++
            s"bind: ${bind}\n"))
      }
    }
  }

  def forMap(
    srcMap: MultiMap[String, Value],
    tarMap: MultiMap[String, Value],
    bind: Bind,
  ): Either[ErrorMsg, Bind] = {
    val initResult: Either[ErrorMsg, Bind] =
      Right(bind)

    def updateBind(
      bind: Bind,
      name: String,
      tarValue: Value,
    ): Either[ErrorMsg, Bind] = {
      srcMap.get(name) match {
        case Some(srcValue) =>
          unify(srcValue, tarValue, bind)
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
    assert(bind1.keys.toSet.intersect(bind2.keys.toSet).size == 0)
    Right(bind2 ++ bind1)
  }

//   def forBind(
//     bind1: Bind,
//     bind2: Bind,
//   ): Either[ErrorMsg, Bind] = {
//     val initBind = bind1 ++ bind2.filterNot { case (id, _value) =>
//       bind1.keys.toSet.contains(id) }

//     assert(initBind.size >= bind1.size)

//     val initResult: Either[ErrorMsg, Bind] =
//       Right(initBind)

//     def updateBind(bind: Bind, id: Id, v: Value): Either[ErrorMsg, Bind] = {
//       bind.get(id) match {
//         case Some(v1) =>
//           unify(v1, v, bind)
//         case None =>
//           Left(ErrorMsg(s"forBind internal error, bind1: ${bind1}, bind2: ${bind2}"))
//       }
//     }

//     bind2
//       .filter { case (id, _value) => bind1.keys.toSet.contains(id) }
//       .foldLeft(initResult) { case (result, (id, v)) =>
//         result.flatMap { bind => updateBind(bind, id, v) }
//       }
//   }
}
