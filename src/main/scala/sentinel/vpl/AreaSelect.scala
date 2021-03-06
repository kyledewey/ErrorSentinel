/*
 * AreaSelect.scala
 */

package sentinel.vpl

/**
 * Describes a rectangular box, with a top left corner and a bottom
 * right corner.
 * @param startX Top left corner of the rectangle
 * @param startY Top left corner of the rectangle
 * @param endX Bottom right corner of the rectangle
 * @param endY Bottom right corner of the rectangle
 * @author Kyle Dewey
 */
class Rectangle( var startX: Int,
		 var startY: Int,
		 var endX: Int,
		 var endY: Int ) {
  /**
   * Determines if this rectangle equals another.
   * This means that the startX, startY, endX, and endY are
   * all the same.
   * @param other The other object to compare to
   * @return true if all the data is the same, else false
   */
  override def equals( other: Any ) = 
    if ( other.isInstanceOf[ Rectangle ] ) {
      val asRect = other.asInstanceOf[ Rectangle ]
      ( startX == asRect.startX &&
        startY == asRect.startY &&
        endX == asRect.endX &&
        endY == asRect.endY )
    } else false

  /**
   * Returns this rectangle as a string.
   * The format is as so:
   * "( ( startX, startY ) ( endX, endY ) )"
   * @return A string in the above format
   */
  override def toString() =
    "( ( " + startX + ", " + startY + " ) " +
    "( " + endX + ", " + endY + " ) )"
}

/**
 * Represents a selection state for a rectangle.
 * Picture a typical four quadrant cartesian plane. Imagine that
 * the rectangle sits in the bottom right quadrant. Depending on what
 * quandrant the user is dragging the mouse in, our behavior changes as
 * to how to resize the rectangle
 * @author Kyle Dewey
 */
trait SelectionRectangleState {
  /**
   * Adjusts the values of the rectangle, given the new
   * x and y coordinates of the mouse.  Note that this may
   * also change the state of the rectangle, depending on what the
   * new values are
   * @param rect The rectangle to manipulate
   * @param newX The new x value
   * @param newY The new y value
   */
  def adjustRectangle( rect: SelectionRectangle, newX: Int, newY: Int ): Unit
}

/**
 * State where the user is dragging in the bottom right quadrant
 * @author Kyle Dewey
 */
object BottomRightState extends SelectionRectangleState {
  /**
   * As long as the new point is completely >= the start point, this
   * merely swaps the values of the end point with this new point.  If not,
   * then it will change to the appropriate state.
   * @param rect The rectangle to manipulate
   * @param newX The new x value
   * @param newY The new y value
   */
  def adjustRectangle( rect: SelectionRectangle, newX: Int, newY: Int ) {
    if ( newX < rect.startX &&
	 newY < rect.startY ) {
      rect.endX = rect.startX
      rect.endY = rect.startY
      rect.startX = newX
      rect.startY = newY
      rect.changeState( TopLeftState )
    } else if ( newX < rect.startX ) {
      rect.endX = rect.startX
      rect.startX = newX
      rect.endY = newY
      rect.changeState( BottomLeftState )
    } else if ( newY < rect.startY ) {
      rect.endY = rect.startY
      rect.startY = newY
      rect.endX = newX
      rect.changeState( TopRightState ) 
    } else {
      rect.endX = newX
      rect.endY = newY
    }
  }
}

/**
 * State where the user is dragging in the bottom left quadrant.
 * @author Kyle Dewey
 */
object BottomLeftState extends SelectionRectangleState {
  /**
   * Adjusts the rectangle's value appropriately for being in
   * the bottom left quadrant.
   * @param rect The underlying rectangle
   * @param newX The new x value
   * @param newY The new y value
   */
  def adjustRectangle( rect: SelectionRectangle, newX: Int, newY: Int ) {
    if ( newX > rect.endX &&
	 newY < rect.startY ) {
      rect.startX = rect.endX
      rect.endY = rect.startY
      rect.endX = newX
      rect.startY = newY
      rect.changeState( TopRightState )
    } else if ( newY < rect.startY ) {
      rect.startY = newY
      rect.startX = newX
      rect.changeState( TopLeftState )
    } else if ( newX > rect.endX ) {
      rect.startX = rect.endX
      rect.endX = newX
      rect.endY = newY
      rect.changeState( BottomRightState )
    } else {
      rect.startX = newX
      rect.endY = newY
    }
  }
}

/**
 * State where the user is dragging in the top left quadrant
 * @author Kyle Dewey
 */
object TopLeftState extends SelectionRectangleState {
  /**
   * Adjusts the rectangle's values appropriately for being in the top
   * left quadrant.
   * @param rect The underlying rectangle
   * @param newX The new x value
   * @param newY The new y value
   */
  def adjustRectangle( rect: SelectionRectangle, newX: Int, newY: Int ) {
    if ( newX > rect.endX &&
	 newY > rect.endY ) {
      rect.startX = rect.endX
      rect.startY = rect.endY
      rect.endX = newX
      rect.endY = newY
      rect.changeState( BottomRightState )
    } else if ( newX > rect.endX ) {
      rect.startX = rect.endX
      rect.endX = newX
      rect.startY = newY
      rect.changeState( TopRightState )
    } else if ( newY > rect.endY ) {
      rect.startY = rect.endY
      rect.startX = newX
      rect.endY = newY
      rect.changeState( BottomLeftState )
    } else {
      rect.startY = newY
      rect.startX = newX
    }
  }
}

/**
 * State where the user is dragging in the top right quadrant
 * @author Kyle Dewey
 */
object TopRightState extends SelectionRectangleState {
  /**
   * Adjusts the rectangle's values appropriately for being in the rop right
   * quadrant
   * @param rect The underlying rectangle
   * @param newX The new x value
   * @param newY The new y value
   */
  def adjustRectangle( rect: SelectionRectangle, newX: Int, newY: Int ) {
    if ( newX < rect.startX &&
	 newY > rect.endY ) {
      rect.endX = rect.startX
      rect.startY = rect.endY
      rect.startX = newX
      rect.endY = newY
      rect.changeState( BottomLeftState )
    } else if ( newX < rect.startX ) {
      rect.endX = rect.startX
      rect.startX = newX
      rect.startY = newY
      rect.changeState( TopLeftState )
    } else if ( newY > rect.endY ) {
      rect.startY = rect.endY
      rect.endX = newX
      rect.endY = newY
      rect.changeState( BottomRightState ) 
    } else {
      rect.startY = newY
      rect.endX = newX
    }
  }
}
      
/**
 * Represents a rectangle in a selection.
 * As the user moves the mouse for a selection, the top left and bottom
 * right corners change.  Depending on the direction of the dragging, the way
 * they can change changes significantly, so the state pattern is used.
 * @param startX  Top left corner of the rectangle
 * @param startY Top left corner of the rectangle
 * @param endX Bottom right corner of the rectangle
 * @param endY Bottom right corner of the rectangle
 * @author Kyle Dewey
 */
class SelectionRectangle( startX: Int, startY: Int, endX: Int, endY: Int )
extends Rectangle( startX, startY, endX, endY ) {
  // being instance variables
  private var state: SelectionRectangleState = BottomRightState
  // end instance variables

  /**
   * Creates a new rectangle that has an area of 0.  The endX and endY
   * values are the same as the start x and start y values
   * @param x The x value to use for the start and end
   * @param y The y value to use for the start and end
   */
  def this( x: Int, y: Int ) =
    this( x, y, x, y )

  /**
   * Changes the state of the rectangle.
   * That is, the quadrant in which the user is dragging changed
   * @param state The new state
   */
  def changeState( state: SelectionRectangleState ) {
    this.state = state
  }

  /**
   * Adjusts the rectangle's values, given the new x and y coordinates
   * @param newX The new x value
   * @param newY The new y value
   */
  def adjustRectangle( newX: Int, newY: Int ) {
    state.adjustRectangle( this, newX, newY )
  }
}
