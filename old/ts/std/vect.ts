import * as ut from "../../util"
import * as cc from "../core"
import { union_builder_t } from "../union"
import { record_builder_t } from "../record"
import { type_t } from "../type-of-type"
import { ref_t } from "../ref"
import { nat } from "./nat"

let m = new cc.module_t ("vect")

// m.use (nat)

m.define ("vect_t", new union_builder_t ("vect_t", [
  new ref_t ("vect_null_t"),
  new ref_t ("vect_cons_t"),
], map => ({
  t: new type_t (),
  length: new ref_t ("nat_t"),
})))

m.define ("vect_null_t", new record_builder_t ("vect_null_t", map => ({
  t: new type_t (),
  length: new ref_t ("zero_t"),
})))

// m.define ("vect_cons_t", new record_builder_t ("vect_cons_t", map => ({
//   [implicit]: {
//     n: new ref_t ("nat_t"),
//   },
//   t: new type_t (),
//   length: m.game ("succ_t") .choices ({
//     prev: new this_t (map, "n"),
//   }),
//   car: new this_t (map, "t"),
//   cdr: m.game ("vect_t") .choices ({
//     t: new this_t (map, "t"),
//     length: new this_t (map, "n"),
//   }),
// })))

export { m as vect }
