package xieyuheng.cicadascript

/** Game semantics framework for logic and type system. */

sealed trait Player {
  def opponent: Player = {
    this match {
      case Verifier => Falsifier
      case Falsifier => Verifier
    }
  }
}

final case object Verifier extends Player
final case object Falsifier extends Player

trait Game[+A, C] {
  def choices(player: Player): Seq[C]
  def choose(choice: C): Game[A, C]

  def loser(player: Player): Boolean =
    choices(player).length == 0

  def end: Boolean =
    loser(Verifier) || loser(Falsifier)
}
