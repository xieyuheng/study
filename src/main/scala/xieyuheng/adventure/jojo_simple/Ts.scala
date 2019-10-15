package xieyuheng.adventure.jojo_simple

case class Ts(list: List[Ty] = List()) {

  def length: Int = list.length

  def empty_p(): Boolean = {
    list.length == 0
  }

  def toc(): Option[Ty] = {
    if (list.length == 0) {
      None
    } else {
      Some(list(0))
    }
  }

  def drop(): Ts = {
    Ts(list.tail)
  }

  def push(t: Ty): Ts = {
    Ts(t :: list)
  }

  def push_list(list: List[Ty]): Ts = {
    Ts(list.reverse ++ this.list)
  }
}
