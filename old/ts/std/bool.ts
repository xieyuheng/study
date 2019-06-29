import * as ut from "../../util"
import * as cc from "../core"
import { ref_t } from "../ref"
import { union_builder_t } from "../union"
import { record_builder_t } from "../record"

let m = new cc.module_t ("bool")

m.define ("bool_t", new union_builder_t ("bool_t", [
  new ref_t ("true_t"),
  new ref_t ("false_t"),
]))

m.define ("true_t", new record_builder_t ("true_t"))

m.define ("false_t", new record_builder_t ("false_t"))

export { m as bool }
