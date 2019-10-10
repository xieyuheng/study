package xieyuheng.adventure.untyped_minimal

case class Ds(list: List[Val] = List()) {

  def length: Int = list.length

  def empty_p(): Boolean = {
    list.length == 0
  }

  def toc(): Option[Val] = {
    if (list.length == 0) {
      None
    } else {
      Some(list(0))
    }
  }

  def drop(): Ds = {
    Ds(list.tail)
  }

  def push(value: Val): Ds = {
    Ds(value :: list)
  }

}
