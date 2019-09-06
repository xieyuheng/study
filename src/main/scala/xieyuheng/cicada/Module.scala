package xieyuheng.cicada

import xieyuheng.cicada.pretty._

abstract class Module {
  var env: Env = Env()

  def define_value(
    name: String,
    value: Value,
  ): Unit = {
    env = env.extend(name -> DefineValue(name, value))
  }

  def define_member_type(
    name: String,
    map: MultiMap[String, Exp],
    superName: String,
  ): Unit = {
    env = env.extend(name -> DefineMemberType(name, map, superName))
  }

  def define_sum_type(
    name: String,
    map: MultiMap[String, Exp],
    memberNames: List[String],
  ): Unit = {
    env = env.extend(name -> DefineSumType(name, map, memberNames))
  }

  def define_type(
    name: String,
    fields: MultiMap[String, Exp],
    members: MultiMap[String, MultiMap[String, Exp]],
  ): Unit = {
    define_sum_type(name, fields, members.keys.toList)
    members.entries.foreach { case (memberName, map) =>
      define_member_type(memberName, fields.merge(map), name)
    }
  }

  def define_fn(
    name: String,
    args: MultiMap[String, Exp],
    ret: Exp,
    body: Exp,
  ): Unit = {
    env = env.extend(name -> DefineFn(name, args, ret, body))
  }

  def import_all(that: Module): Unit = {
    that.env.map.foreach { case (name, newDefine) =>
      env.get(name) match {
        case Some(oldDefine) =>
          if (newDefine != oldDefine) {
            println("[warn]")
            println(s"- redefining:\n${addIndentToBlock(prettyDefine(newDefine), 1)}")
            println(s"- old definition:\n${addIndentToBlock(prettyDefine(oldDefine), 1)}")
          }
        case None => {}
      }
      env = env.extend(name -> newDefine)
    }
  }

}
