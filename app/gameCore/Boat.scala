package gameCore

/** class that stores information about a boat to easily pass it to methods
  * @param sX first x coordinate
  * @param sY first y coordinate
  * @param l length of the boat
  * @param dir orientation: horizontal (true) or vertical
  * @param i assign an id to the boat
  */
class Boat(sX: Int, sY: Int, l: Int, dir: Boolean, i: Int) {
	
		/** x coordinate of the upper / left end of the boat */
  	val startX: Int = sX
  	/** y coordinate of the upper / left end of the boat */
  	val startY: Int = sY
  	/** length of the boat */
  	val length: Int = l
  	/** direction of the boat: horizontal or vertical */
  	val isHorizontal: Boolean = dir
  	/** identification number of the boat */
  	val id: Int = i
  
  	/** calculates the x coordinate of the lower / right end of the boat */
  	def getEndX: Int = {
    	if (isHorizontal) {
      		startX + length - 1
    	} else {
      		startX
    	}
  	}
  
  	/** calculates the y coordinate of the lower / right end of the boat */
  	def getEndY: Int = {
    	if (isHorizontal) {
      		startY
    	} else {
      		startY + length - 1
    	}
  	}

}