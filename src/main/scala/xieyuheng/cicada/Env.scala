package xieyuheng.cicada

case class Env(val defMap: Map[String, Def] = Map()) {
  def get(name: String): Option[Def] = {
    defMap.get(name)
  }

  def contains(name: String): Boolean = {
    get(name).isDefined
  }

  def extend(kv: (String, Def)): Env = {
    Env(defMap + kv)
  }

  def extendByValueMap(valueMap: MultiMap[String, Value]): Env = {
    valueMap.entries.foldLeft(this) { case (env, (name, value)) =>
      env.extend(name -> DefineValue(name, value))
    }
  }

  def defineValue(
    name: String,
    value: Value,
  ): Env = {
    extend(name -> DefineValue(name, value))
  }

  def define(
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

  def defineRecord(
    name: String,
    map: MultiMap[String, Exp],
  ): Env = {
    extend(name -> DefineRecord(name, map))
  }

  def defineUnion(
    name: String,
    map: MultiMap[String, Exp],
    subNames: List[String],
  ): Env = {
    extend(name -> DefineUnion(name, map, subNames))
  }

  def importAll(that: Env): Env = {
    that.defMap.foldLeft(this) { case (env, (name, value)) =>
      env.get(name) match {
        case Some(oldValue) =>
          println(s"- [WARN] re-defining name: ${name} to: ${value}")
          println(s"  old value: ${oldValue}")
        case None => {}
      }
      env.extend(name -> value)
    }
  }
}

object Env {
  def merge(seq: Seq[Env]): Env = {
    seq.foldLeft(Env()) { case (env, e) =>
      env.importAll(e)
    }
  }
}
