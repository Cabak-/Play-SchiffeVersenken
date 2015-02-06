package models
/** an exception for invalid turns by players */
class InvalidActionException extends Exception {
}

/** an exception for the event when a player proceeds an action
  * although it is not his turn */
class NotPlayersTurnException extends InvalidActionException {
}

/** an exception for the event when a player wants to place a boat
  * after his placement phase was already finished */
class PlacementFinishedException extends InvalidActionException {
}

/** an exception for the event when a player shoots before the
  * placement phase was finished */
class PlacementNotFinishedException extends InvalidActionException {
}

/** an exception for the event when a player tries to shoot
  * outside the field */
class ShotNotInFieldException extends InvalidActionException {
}

/** an exception for the event when a player shoots at a cell
  * that has already been uncovered */
class ShotOnUncoveredCellException extends InvalidActionException {
}