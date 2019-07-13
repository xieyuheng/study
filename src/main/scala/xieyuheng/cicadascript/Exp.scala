package xieyuheng.cicadascript

trait Exp

case class LogicVar(name: String) extends Exp

/*
 we can unify LogicVar with any Value
 but we must also be able to unify fulfilling relation, such as

 Value <: LogicVar
 Lambda <: Pi
 Record <: Union

 how can we achieve this ?
 we did this once by mapping between such objects.

 we can use union find algorithm

 this means each object must has it own unique id.
 */

/*
 t2 <- ctx.loopupLogicVar(name)
 ctx :- t2 <: t
 ---------------
 ctx :- LogicVar(name) <: t
 */

case class Union(name: String, map: Map[String, Exp], subNames: List[String]) extends Exp
case class Case(target: Exp, clauses: Map[String, Exp]) extends Exp

/*
 target must be Union,
 and clauses.keys == target.subNames

 this means we should generate a eliminator
 (like `fold`) for every Union
 (but we are using unification and `.field`
 instead of pattern matching)

 foreach name and body in clauses {
 | sub <- ctx.loopupValue(name)
 | ctx :- target :> sub
 | ctx.unify(target, sub) :- body <: t
 }
 --------------------
 ctx :- Case(target, clauses) <: t
 */

case class Record(name: String, map: Map[String, Exp]) extends Exp
case class Field(head: Exp, fieldName: String) extends Exp

/*
 head can be Record or Union

 ctx :- head.map.get(fieldName) <: t
 ----------------
 ctx :- Field(head, fieldName) <: t
 */

/*
 Record <: Union

 subNames contains name
 ctx :- map <: map2
 -------------------
 ctx :- Record(name, map) <: Union(name2, map2, subNames)
 */

case class Pi(args: Map[String, Exp], ret: Exp) extends Exp
case class Lambda(args: Map[String, Exp], ret: Exp, body: Exp) extends Exp
case class Apply(fun: Exp, args: Map[String, Exp]) extends Exp

/*
 fun can be Lambda or Pi

 ctx :- args <: fun.args
 ctx.unify(args, fun.args) :- fun.ret <: t
 --------------------
 ctx :- Apply(fun, args) <: t
 */

/*
 Lambda <: Pi

 ctx :- args <: args2
 ctx :- ret <: ret2
 --------------------
 ctx :- Lambda(args, ret, body) <: Pi(args2, ret2)
 */
