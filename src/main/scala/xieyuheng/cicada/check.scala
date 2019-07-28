package xieyuheng.cicada

import scala.collection.immutable.ListMap

object check {
  def check(exp: Exp, t: Value) = {
    ???
    // - target must be Union,
    //   and clauses.keys == target.subNames
    // - this means we should generate a eliminator
    //   (like `fold`) for every Union
    //   (but we are using unification and `.field`
    //   instead of pattern matching)

    //   foreach name and body in clauses {
    //     sub <- ctx.loopupValue(name)
    //     ctx :- target :> sub
    //     ctx.fulfill(target, sub) :- body <: t
    //   }
    //   --------------------
    //   ctx :- Case(target, clauses) <: t

    // - target can be Record or Union

    //   ctx :- target.map.get(fieldName) <: t
    //   ----------------
    //   ctx :- Field(target, fieldName) <: t

    // - Apply

    //   fun can be Fn or Pi
    //   ctx :- args <: target.args
    //   ctx.fulfill(args, target.args) :- target.ret <: t
    //   --------------------
    //   ctx :- Apply(target, args) <: t
  }
}
