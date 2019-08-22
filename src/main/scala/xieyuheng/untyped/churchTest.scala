package xieyuheng.untyped

import example.Church

object churchTest extends App {
  val `Church.m should run Church numerals` = {
    val m = Church.m
    m.run(Church.fromInt(0))
    m.run(Church.fromInt(1))
    m.run(Church.fromInt(2))
    m.run(Church.fromInt(3))
    m.run(Apply(Apply(Var("church_add"), Church.fromInt(2)), Church.fromInt(2)))
  }
}
