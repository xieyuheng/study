package xieyuheng.adventure.jojo_counted

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

  def toc_ext_env(name: String, entry: EnvEntry): Rs = {
    val frame = list.head
    val new_env = frame.env.ext(name, entry)
    Rs(frame.copy(env = new_env) :: list.tail)
  }

  def next(): Rs = {
    val frame = list.head
    val new_frame = frame.copy(index = frame.index + 1)
    drop().push(new_frame)
  }

}
