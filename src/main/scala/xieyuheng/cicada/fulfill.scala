package xieyuheng.cicada

import scala.collection.immutable.ListMap

object fulfill {
  def apply(src: Value, tar: Value, bind: Bind): Either[ErrorMsg, Bind] = {
    (util.walk(src, bind), util.walk(tar, bind)) match {
      case (src, tar) if src == tar => {
        Right(bind)
      }

      case (value, t: LogicVar) => {
        Right(bind + (t.id -> value))
      }

      case (fn: FnValue, pi: PiValue) => {
        for {
          /** contravariant at args */
          bind1 <- forMap(pi.args, fn.args, bind)
          bind2 <- apply(fn.ret, pi.ret, bind1)
        } yield bind2 + (pi.id -> fn)
      }

      case (record: RecordValue, union: UnionValue) if union.subNames contains record.name => {
        for {
          bind1 <- mergeBind(bind, record.bind)
          bind2 <- mergeBind(bind1, union.bind)
          bind3 <- forMap(record.map, union.map, bind2)
        } yield bind3 + (union.id -> record)
      }

      case (src: UnionValue, tar: UnionValue) if src.name == tar.name => {
        for {
          bind1 <- mergeBind(bind, src.bind)
          bind2 <- mergeBind(bind1, tar.bind)
          result <- forMap(src.map, tar.map, bind2)
        } yield result
      }

      case (src: RecordValue, tar: RecordValue) if src.name == tar.name => {
        for {
          bind1 <- mergeBind(bind, src.bind)
          bind2 <- mergeBind(bind1, tar.bind)
          result <- forMap(src.map, tar.map, bind2)
        } yield result
      }

      case (src: PiValue, tar: PiValue) => {
        for {
          bind1 <- forMap(tar.args, src.args, bind)
          bind2 <- apply(src.ret, tar.ret, bind1)
        } yield bind2
      }

      case _ => {
        Left(ErrorMsg(s"fail to fulfill src: ${src} into tar: ${tar}"))
      }
    }
  }

  def forMap(
    srcMap: ListMap[String, Value],
    tarMap: ListMap[String, Value],
    bind: Bind,
  ): Either[ErrorMsg, Bind] = {
    val initResult: Either[ErrorMsg, Bind] = Right(Map())
    tarMap.foldLeft(initResult) { case (result, (name, tarValue)) =>
      for {
        bind1 <- result
        bind2 <- srcMap.get(name) match {
          case Some(srcValue) =>
            apply(srcValue, tarValue, bind1)
          case None =>
            Left(ErrorMsg(s"srcMap does not have name: ${name}, tarValue: ${tarValue}"))
        }
      } yield bind2
    }
  }

  def mergeBind(
    bind1: Bind,
    bind2: Bind,
  ): Either[ErrorMsg, Bind] = {
    assert(bind1.keys.toSet.intersect(bind2.keys.toSet).isEmpty)
    Right(bind1 ++ bind2)
  }
}
