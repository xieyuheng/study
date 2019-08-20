package xieyuheng.cicada

object check {
  def check(exp: Exp, t: Value) = {
    ???

    // - target must be Union,
    //   and clauses.keys == target.memberNames
    // - this means we should generate a eliminator
    //   (like `fold`) for every Union
    //   (but we are using unification and `.field`
    //   instead of pattern matching)

    //   foreach name and body in clauses {
    //     sub <- ctx.loopupValue(name)
    //     ctx :- target :> sub
    //     ctx.unify(target, sub) :- body <: t
    //   }
    //   --------------------
    //   ctx :- Case(target, clauses) <: t


    // - target can be Record or Union

    //   ctx :- target.map.get(fieldName) <: t
    //   ----------------
    //   ctx :- Field(target, fieldName) <: t


    // - Ap

    //   target can be Fn or Pi

    //   target can be Union or Record

    //   ctx :- args <: target.args
    //   ctx.unify(args, target.args) :- target.ret <: t
    //   --------------------
    //   ctx :- Ap(target, args) <: t
  }
}
