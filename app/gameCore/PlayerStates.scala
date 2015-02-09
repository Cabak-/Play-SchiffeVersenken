package gameCore

/** enumeration for possible states of players */
object PlayerStates extends Enumeration {
  type PlayerStates = Value
  val PLACING_BOATS, WAITING_FOR_PLACEMENT, SHOOTING, WAITING_FOR_SHOT = Value
}
