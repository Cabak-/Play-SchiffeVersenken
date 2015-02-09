package gameCore

/** immutable class to keep track of the non-static information about a game field,
  * i.e. which cells have been uncovered by shots and how many boats are left
  * @param fieldSize size of the field
  * @param boatCount amount of boats of length i
  * @param totalBoatCount total amount of boats
  * @param boatsLeft amount of boats of length i that have not been destroyed yet
  * @param visualCells visible state of the cells: have not been shot yet / hit a boat / hit water
  */
class BasicFieldState(val fieldSize: Int, val boatCount: IndexedSeq[Int], val totalBoatCount: Int,
                      val boatsLeft: IndexedSeq[Int], val visualCells: IndexedSeq[Int]) {

  /** gets the cell value at given coordinates */
  def getVisualCell(x:Int,y:Int): Int = {
    return visualCells(y*fieldSize+x)
  }

	/** uncovers a cell on the field in the game state */
	def uncoverCell(x: Int, y: Int, id: Int): BasicFieldState = {
		//visualCells(x)(y) = id
		if (id == -1) {
      return new BasicFieldState(fieldSize,boatCount,totalBoatCount,boatsLeft,visualCells.updated(y*fieldSize+x, -1))
			//visualCells(x)(y) = -1
		} else {
      return new BasicFieldState(fieldSize,boatCount,totalBoatCount,boatsLeft,visualCells.updated(y*fieldSize+x, 1))
			//visualCells(x)(y) = 1
		}
	}

	/** checks if all boats have been sunk */
	def isFinished: Boolean = {
		for (i <- 1 until boatsLeft.size) {
      //println("BOATS OF LENGTH " + i + " LEFT: " + boatsLeft(i))
			if (boatsLeft(i) > 0) return false
		}
		return true
	}
	
	/** outputs the GameState on the console */
	def show{
		println("------------------------")
		for (y <- 0 until fieldSize) {
			for (x <- 0 until fieldSize) {
				print(" " + getVisualCell(x,y))
			}
			println()
		}
		println("------------------------")
	}

}


/** extension of the field state class for states where the life
	* of each boat is also known and the instance of the program is also
	* able to tell if a boat was destroyed; contains a game area
  * (-> for servers or the client of the player who owns the field)
  * @param fieldSize size of the field
  * @param boatCount amount of boats of length i
  * @param totalBoatCount total amount of boats
  * @param boatsLeft amount of boats of length i that have not been destroyed yet
  * @param visualCells visible state of the cells: have not been shot yet / hit a boat / hit water
  * @param boatLife remaining life of the boats, referenced by their IDs
  * @param gameArea the game area with the boat placements belonging to the field state
  * @param shotResult result of the last shot (0: water, 1: hit, 2: sunk, 3: won)
  */
class FieldState(fieldSize: Int, boatCount: IndexedSeq[Int], totalBoatCount: Int,boatsLeft: IndexedSeq[Int],
                 visualCells: IndexedSeq[Int], val boatLife: IndexedSeq[Int], val gameArea: GameArea, val shotResult: Int)
  extends BasicFieldState(fieldSize,boatCount,totalBoatCount,boatsLeft,visualCells) {

  /** changes the game area to place a new boat */
  def placeBoat(b: Boat): FieldState = {
    return new FieldState(fieldSize,boatCount,totalBoatCount,boatsLeft,visualCells,boatLife:+(b.length),gameArea.placeBoat(b),-1)
  }

	/** makes a change in the game state resulted by a shot at a cell,
		* returns 0 for water */
	def registerShot(x: Int, y: Int): FieldState = {
		if (getVisualCell(x,y) != 0) { // error: cell was already uncovered
			throw new ShotOnUncoveredCellException
		} else {
			// determine the id of the boat, if a boat was hit
			val id: Int = gameArea.checkCell(x,y)
			if (id > 0) { // boat was hit
				// mark the cell
				val newCells: IndexedSeq[Int] = visualCells.updated(y*fieldSize+x, 1)

				// update the boat life
        val newBoatLife: IndexedSeq[Int] = boatLife.updated(id-1, boatLife(id-1) - 1)

				// check if the boat has been sunk
        val newBoatsLeft: IndexedSeq[Int] = {
          if (newBoatLife(id - 1) > 0) boatsLeft
          else boatsLeft.updated(gameArea.boatObjects(id-1).length,boatsLeft(gameArea.boatObjects(id-1).length) - 1)
        };

        // function to check immediately if the game is finished
        def allBoatsDestroyed(left: IndexedSeq[Int]): Boolean= {
          for (i <- 1 until left.size) {
            if (left(i) > 0) return false
          }
          return true
        }

        // get the shot result
        val newShotResult: Int = {
          if (newBoatLife(id - 1) > 0) 1
          else if (allBoatsDestroyed(newBoatsLeft)) 3
          else 2
        };

        return new FieldState(fieldSize,boatCount,totalBoatCount,newBoatsLeft,newCells,newBoatLife,gameArea,newShotResult)
			} else {
				// water was hit
        // mark the cell
        val newCells: IndexedSeq[Int] = visualCells.updated(y*fieldSize+x, -1)
        val newShotResult: Int = 0
        return new FieldState(fieldSize,boatCount,totalBoatCount,boatsLeft,newCells,boatLife,gameArea,newShotResult)

			}
		}
	}

}