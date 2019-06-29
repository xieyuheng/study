import assert from "assert"
import * as ut from "../util"
import * as gs from "./game-semantics"
import { step_t } from "./path"
import { ref_t } from "./ref"
import { record_t } from "./record"
import { union_t } from "./union"
import { module_t } from "./core"

export
class field_t extends step_t {
  name: string

  constructor (
    name: string
  ) {
    super ()
    this.name = name
  }

  forward (game: gs.game_t): gs.game_t {
    if (game instanceof record_t) {
      let record = game
      let next_game = record.map.get (this.name)
      if (next_game === undefined) {
        throw new Error (`unknown field name: ${this.name}`)
      } else {
        return next_game
      }
    } else if (game instanceof union_t) {
      let union = game
      let next_game = union.map.get (this.name)
      if (next_game === undefined) {
        throw new Error (`unknown field name: ${this.name}`)
      } else {
        return next_game
      }
    } else {
      throw new Error ("field_t step only forward a record_t or union_t")
    }
  }

  deref (m: module_t, game: gs.game_t) {
    if (game instanceof record_t) {
      let record = game
      let next_game = record.map.get (this.name)
      if (next_game === undefined) {
        throw new Error (`unknown field name: ${this.name}`)
      } else if (next_game instanceof ref_t) {
        let ref = next_game
        record.map.set (this.name, ref.deref (m))
      } else {
        throw new Error (`can not deref a non ref_t`)
      }
    } else if (game instanceof union_t) {
      let union = game
      let next_game = union.map.get (this.name)
      if (next_game === undefined) {
        throw new Error (`unknown field name: ${this.name}`)
      } else if (next_game instanceof ref_t) {
        let ref = next_game
        union.map.set (this.name, ref.deref (m))
      } else {
        throw new Error (`can not deref a non ref_t`)
      }
    } else {
      throw new Error ("field_t step only deref a record_t or union_t")
    }
  }

  repr (): any {
    return this.name
  }
}
