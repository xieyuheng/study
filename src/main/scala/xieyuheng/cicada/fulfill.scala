package xieyuheng.cicada

import scala.annotation.tailrec
import scala.collection.immutable.ListMap

object fulfill {
  @tailrec
  def walk(x: Value, bind: Ctx.Bind): Value = {
    bind.get(x) match {
      case Some(y) => walk(y, bind)
      case None => x
    }
  }

  def fulfill
    (src: Value, tar: Value, bind: Ctx.Bind)
      : Either[ErrorMsg, Ctx.Bind] = {
    (walk(src, bind), walk(tar, bind)) match {
      case (src, tar) if src == tar => {
        Right(bind)
      }

      case (value, t: TypeValue) => {
        Right(bind + (t -> value))
      }

      case (fn: FnValue, pi: PiValue) => {
        for {
          /** contravariant at args */
          bind1 <- fulfillMap(pi.args, fn.args, bind)
          bind2 <- fulfill(fn.ret, pi.ret, bind1)
        } yield bind2 + (fn -> pi)
      }

      case (record: RecordValue, union: UnionValue) if union.subNames contains record.name => {
        for {
          bind1 <- mergeBind(bind, record.bind)
          bind2 <- mergeBind(bind1, union.bind)
          bind3 <- fulfillMap(record.map, union.map, bind2)
        } yield bind3 + (record -> union)
      }

      case (src: UnionValue, tar: UnionValue) if src.name == tar.name => {
        for {
          bind1 <- mergeBind(bind, src.bind)
          bind2 <- mergeBind(bind1, tar.bind)
          result <- fulfillMap(src.map, tar.map, bind2)
        } yield result
      }

      case (src: RecordValue, tar: RecordValue) if src.name == tar.name => {
        for {
          bind1 <- mergeBind(bind, src.bind)
          bind2 <- mergeBind(bind1, tar.bind)
          result <- fulfillMap(src.map, tar.map, bind2)
        } yield result
      }

      case (src: PiValue, tar: PiValue) => {
        for {
          bind1 <- fulfillMap(tar.args, src.args, bind)
          bind2 <- fulfill(src.ret, tar.ret, bind1)
        } yield bind2
      }

      case _ => {
        Left(ErrorMsg(s"fail to fulfill src: ${src} into tar: ${tar}"))
      }
    }
  }

  def fulfillMap
    (srcMap: ListMap[String, Value],
      tarMap: ListMap[String, Value],
      bind: Ctx.Bind)
      : Either[ErrorMsg, Ctx.Bind] = {
    val initResult: Either[ErrorMsg, Ctx.Bind] = Right(Map())
    tarMap.foldLeft(initResult) { case (result, (name, tarValue)) =>
      for {
        bind1 <- result
        bind2 <- srcMap.get(name) match {
          case Some(srcValue) =>
            fulfill(srcValue, tarValue, bind1)
          case None =>
            Left(ErrorMsg(s"srcMap does not have name: ${name}, tarValue: ${tarValue}"))
        }
      } yield bind2
    }
  }

  def mergeBind
    (bind1: Ctx.Bind,
      bind2: Ctx.Bind)
      : Either[ErrorMsg, Ctx.Bind] = {
    assert(bind1.keys.toSet.intersect(bind2.keys.toSet).isEmpty)
    Right(bind1 ++ bind2)
  }
}
