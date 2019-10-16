package xieyuheng.eopl.lang_call_by_need

import scala.annotation.tailrec

final case class Store(map: Map[Int, Val] = Map()) {

  @tailrec
  def fresh_address_from(address: Int): Int = {
    if (map.keySet.contains(address)) {
      fresh_address_from(address + 1)
    } else {
      address
    }
  }

  def ref_get(address: Int): Option[Val] = {
    map.get(address)
  }

  def ref_set(address: Int, value: Val): Store = {
    Store(map + (address -> value))
  }

  def fresh_address(): Int = {
    fresh_address_from(0)
  }

  def ref_new(value: Val): (Store, Int) = {
    val address = fresh_address()
    val new_store = ref_set(address, value)
    (new_store, address)
  }

}
