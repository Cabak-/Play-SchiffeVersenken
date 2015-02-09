package gameCore

/** an exception for invalid placements of boats */
class InvalidPlacementException extends Exception {
}

/** an exception that will be thrown if a boat is about to be placed
  * outside of the GameArea */
class BoatOverhangException extends InvalidPlacementException {
}

/** an exception that will be thrown if a boat is about to be placed
  * too close to another boat */
class BoatOverlapException extends Exception {
}