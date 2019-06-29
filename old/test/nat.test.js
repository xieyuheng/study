import test from "ava"

import * as cc from "../../lib/cicada/core"
import * as ut from "../../lib/util"

import * as prelude from "../../lib/cicada/prelude"

test ("nat_t", t => {
  let m = prelude.nat
  m .game ("nat_t") .info (0)
    .choose (m, cc.path ([
      cc.step.member ("succ_t")
    ])) .info (1)
    .choose (m, cc.path ([
      cc.step.member ("succ_t"),
      cc.step.field ("prev"),
    ])) .info (2)
    .choose (m, cc.path ([
      cc.step.member ("succ_t"),
      cc.step.field ("prev"),
      cc.step.member ("succ_t"),
    ])) .info (3)
    .choose (m, cc.path ([
      cc.step.member ("succ_t"),
      cc.step.field ("prev"),
      cc.step.member ("succ_t"),
      cc.step.field ("prev"),
    ])) .info (4)
    .choose (m, cc.path ([
      cc.step.member ("succ_t"),
      cc.step.field ("prev"),
      cc.step.member ("succ_t"),
      cc.step.field ("prev"),
      cc.step.member ("zero_t"),
    ])) .info (5)

  t.pass ()
})
