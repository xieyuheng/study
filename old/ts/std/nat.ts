import * as ut from "../../util"
import * as cc from "../core"
import { union_builder_t } from "../union"
import { record_builder_t } from "../record"
import { ref_t } from "../ref"

let m = new cc.module_t ("nat")

m.define ("nat_t", new union_builder_t ("nat_t", [
  new ref_t ("zero_t"),
  new ref_t ("succ_t"),
]))

m.define ("zero_t", new record_builder_t ("zero_t"))

m.define ("succ_t", new record_builder_t ("succ_t", _map => ({
  prev: new ref_t ("nat_t"),
})))

export { m as nat }
