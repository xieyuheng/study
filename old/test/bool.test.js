import test from "ava"

import * as cc from "../../lib/cicada/core"
import * as ut from "../../lib/util"

import * as prelude from "../../lib/cicada/prelude"
import { pi_t } from "../../lib/cicada/pi"
import { ref_t } from "../../lib/cicada/ref"

test ("bool_t", t => {
  let m = prelude.bool
  m .game ("bool_t") .info (0)
    .choose (m, cc.path ([
      cc.step.member ("true_t")
    ])) .info (1)

  t.pass ()
})

test ("f1_t", t => {
  let m = prelude.bool.shallow_copy ()

  m.define ("f1_t", new pi_t ({
    "x": new ref_t ("bool_t") .deref (m),
    "y": new ref_t ("bool_t") .deref (m),
  }, new ref_t ("bool_t") .deref (m)))

  m.game ("f1_t") .info (0)
    .choose (m, cc.path ([
      cc.step.arg ("x"),
      cc.step.member ("true_t"),
    ])) .info (1)
    .choose (m, cc.path ([
      cc.step.arg ("y"),
      cc.step.member ("false_t"),
    ])) .info (2)
    .choose (m, cc.path ([
      cc.step.ret (),
      cc.step.member ("false_t")
    ])) .info (3)

  t.pass ()
})
