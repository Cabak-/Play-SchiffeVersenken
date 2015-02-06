package models
/** enumeration for messages passed to the player (not exceptions) */
object PlayerMessages extends Enumeration {
  type PlayerMessages = Value
  val BOAT_PLACED, WATER, HIT, SUNK, YOU_WIN, YOU_LOSE = Value
}
