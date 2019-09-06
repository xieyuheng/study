package xieyuheng.cicada.prelude

import xieyuheng.cicada._
import xieyuheng.cicada.expDSL._
import xieyuheng.cicada.pretty._
import xieyuheng.cicada.json.rw._

import upickle.default._

object nat_test extends App {
  import nat._

  implicit val module = nat.env

  util.evalOnRight("succ_t" ap %("prev" -> "zero_t")) {
    case value =>
      assert(nat.to_int(value) == 1)
  }

  util.evalOnRight("succ_t" ap %("prev" -> "zero_t") dot "prev") {
    case value =>
      assert(nat.to_int(value) == 0)
  }

  util.evalOnRight("nat_add" ap %("x" -> one, "y" -> one)) {
    case value =>
      assert(nat.to_int(value) == 2)
  }

  util.evalOnRight("nat_add" ap %("x" -> two, "y" -> two)) {
    case value =>
      assert(nat.to_int(value) == 4)
  }
}
