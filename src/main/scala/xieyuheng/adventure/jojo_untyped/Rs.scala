package xieyuheng.adventure.jojo_untyped

case class Frame(index: Int, list: List[Jo], env: Env)

case class Rs(list: List[Frame] = List()) {

  def length: Int = list.length

  def empty_p(): Boolean = {
    list.length == 0
  }

  def toc(): Option[Frame] = {
    if (list.length == 0) {
      None
    } else {
      Some(list(0))
    }
  }

  def drop(): Rs = {
    Rs(list.tail)
  }

  def push(frame: Frame): Rs = {
    Rs(frame :: list)
  }

  def toc_ext_let(name: String, value: Val): Rs = {
    val frame = list.head
    val new_env = frame.env.ext_let(name, value)
    Rs(frame.copy(env = new_env) :: list.tail)
  }

  def toc_ext_define(name: String, value: Val): Rs = {
    val frame = list.head
    val new_env = frame.env.ext_define(name, value)
    Rs(frame.copy(env = new_env) :: list.tail)
  }

  def next(): Rs = {
    val frame = list.head
    if (frame.index == frame.list.length - 1) {
      // NOTE tail call
      drop()
    } else {
      val new_frame = frame.copy(index = frame.index + 1)
      drop()
        .push(new_frame)
    }
  }

}
