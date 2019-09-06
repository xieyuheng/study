package xieyuheng.cicada

import scala.collection.immutable.ListMap

import xieyuheng.cicada.pretty._

case class Env(map: Map[String, Define] = Map())
  (implicit config: EnvConfig = EnvConfig.default) {
  def get(name: String): Option[Define] = {
    map.get(name)
  }

  def contains(name: String): Boolean = {
    get(name).isDefined
  }

  def extend(kv: (String, Define)): Env = {
    Env(map + kv)
  }

  def extendByValueMap(valueMap: ListMap[String, Value]): Env = {
    valueMap.foldLeft(this) { case (env, (name, value)) =>
      env.extend(name -> DefineValue(name, value))
    }
  }

  def define_value(
    name: String,
    value: Value,
  ): Env = {
    extend(name -> DefineValue(name, value))
  }

  def defExp(
    name: String,
    exp: Exp,
  ): Env = {
    eval(exp, this) match {
      case Right(value) =>
        extend(name -> DefineValue(name, value))
      case Left(errorMsg) =>
        println(errorMsg)
        this
    }
  }

  private def define_member_type(
    name: String,
    map: MultiMap[String, Exp],
    superName: String,
  ): Env = {
    extend(name -> DefineMemberType(name, map, superName))
  }

  private def define_sum_type(
    name: String,
    map: MultiMap[String, Exp],
    memberNames: List[String],
  ): Env = {
    extend(name -> DefineSumType(name, map, memberNames))
  }

  def define_type(
    name: String,
    fields: MultiMap[String, Exp],
    members: MultiMap[String, MultiMap[String, Exp]],
  ): Env = {
    val initEnv = define_sum_type(name, fields, members.keys.toList)
    members.entries.foldLeft(initEnv) { case (env, (memberName, map)) =>
      env.define_member_type(memberName, fields.merge(map), name)
    }
  }

  def define_fn(
    name: String,
    args: MultiMap[String, Exp],
    ret: Exp,
    body: Exp,
  ): Env = {
    extend(name -> DefineFn(name, args, ret, body))
  }

  def import_all(that: Env): Env = {
    that.map.foldLeft(this) { case (env, (name, newDefine)) =>
      env.get(name) match {
        case Some(oldDefine) =>
          if (newDefine != oldDefine) {
            if (config.get("on_redefinition") == Some("warn")) {
              println("[warn]")
              println(s"- redefining:\n${addIndentToBlock(prettyDefine(newDefine), 1)}")
              println(s"- old definition:\n${addIndentToBlock(prettyDefine(oldDefine), 1)}")
            }
          }
        case None => {}
      }
      env.extend(name -> newDefine)
    }
  }
}
