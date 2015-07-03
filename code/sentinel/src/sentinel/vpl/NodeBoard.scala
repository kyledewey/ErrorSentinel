/*
 * NodeBoard.scala
 *
 * Version:
 *     $Id: NodeBoard.scala,v 1.8 2011/04/01 03:24:44 kyledewey Exp $
 *
 * Revisions:
 *      $Log: NodeBoard.scala,v $
 *      Revision 1.8  2011/04/01 03:24:44  kyledewey
 *      Moved getTailNodes() to Function.scala.
 *
 *      Revision 1.7  2011/02/27 04:29:17  kyledewey
 *      Refactored for use with debuggers.
 *
 *      Revision 1.6  2011/02/26 04:14:03  kyledewey
 *      Moved the width and height components to Node.
 *
 *      Revision 1.5  2011/02/11 01:24:18  kyledewey
 *      Nodes now call disconnect() before being removed from the board.
 *
 *      Revision 1.4  2011/01/30 05:24:32  kyledewey
 *      Now T must extend AnyRef.
 *
 *      Revision 1.3  2011/01/29 02:46:46  kyledewey
 *      Fixed bug that caused nodes to appear much larger
 *      than they actually were
 *
 *      Revision 1.2  2011/01/28 02:06:38  kyledewey
 *      Slight cleanup of code.
 *
 *      Revision 1.1  2011/01/27 15:52:20  kyledewey
 *      Initial revision
 *
 *
 */

package sentinel.vpl

/**
 * Contains constants for NodeBoard
 * @author Kyle Dewey
 */
object NodeBoard {
  val DEFAULT_WIDTH = 500
  val DEFAULT_HEIGHT = 500

  val DEFAULT_NODE_UNMOVEABLE_MESSAGE = 
    "The given space isn't open for movement"

  /**
   * Given the ending coordinates, it returns a function that will return true
   * when we have gone beyond the end coordinates
   * @param endX The ending X coordinate
   * @param endY The ending Y coordinate
   * @return A function that returns true when its given coordinates are beyond
   * the given ending coordinates
   */
  def makeDoneWhenBeyond( endX: Int, endY: Int ) = {
    ( currentX: Int, currentY: Int ) =>
      ( currentY > endY || 
        ( currentY == endY &&
	  currentX > endX ) )
  }
}

/**
 * Exception thrown when the given node cannot be placed at the given position
 * @param message A message to show the user
 * @author Kyle Dewey
 */
case class NodeUnplacableException( message: String ) extends Exception( message ) {}

/**
 * Exception thrown when an attempt is made to access a node that isn't there.
 * @param message A message to show the user
 * @author Kyle Dewey
 */
case class NoSuchNodeException( message: String ) extends Exception( message ) {}

/**
 * Represents a grid that contains nodes.  Each node is represented as
 * a rectangle, with a constant height and a width determined by
 * the number of inputs.
 * @param width The width of the board
 * @param height The height of the board
 * 
 * @author Kyle Dewey
 */
class NodeBoard[ T <: AnyRef, U ]( val width: Int,
			           val height: Int ) {
  private val board: Array[ Array[ Option[ Node[ T, U ] ] ] ] = 
    new Array( width, height )
  private var corners: Map[ Node[ T, U ], Pair[ Int, Int ] ] = Map()
  foreach( ( x: Int, y: Int ) => board( x )( y ) = None )

  /**
   * Creates a new board using the defaults defined in the NodeBoard object.
   */
  def this() =
    this( NodeBoard.DEFAULT_WIDTH,
	  NodeBoard.DEFAULT_HEIGHT )

  /**
   * Iterates over the board, starting at the given X and Y position.
   * Gives the current X and Y positions to the given function
   * @param startX Starting X
   * @param startY Starting Y
   * @param done Function that determines if we are done or not, given the
   * current X and Y position we are at
   * @param increment Function that returns the next X and Y, given the
   * current X and Y
   * @param function The function to call for the current X and Y position
   */
  def foreach( startX: Int, 
	       startY: Int, 
	       done: ( Int, Int ) => Boolean, 
	       increment: ( Int, Int ) => Pair[ Int, Int ], 
	       function: ( Int, Int ) => Unit ) {

    def foreach( currentX: Int, currentY: Int ) {
      if ( !done( currentX, currentY ) ) {
	val ( nextX,
	      nextY ) = increment( currentX, 
				   currentY )
	function( currentX, currentY )
	foreach( nextX, nextY )
      }
    }

    foreach( startX, startY )
  }

  /**
   * Iterates over all spaces within the board between the two given points.
   * Calls the given function for each of those spaces.
   * @param startX The starting X position
   * @param startY The starting Y position
   * @param endX The ending X position
   * @param endY The ending Y position
   * @param function The function to call for the current x and y position
   */
  def foreach( startX: Int, 
	       startY: Int, 
	       endX: Int, 
	       endY: Int, 
	       function: ( Int, Int ) => Unit ) {
    foreach( startX,
	     startY,
	     NodeBoard.makeDoneWhenBeyond( endX, endY ),
	     ( currentX: Int, currentY: Int ) => {
	       var nextX = currentX + 1
	       var nextY = currentY
	       if ( nextX >= width ) {
		 nextX = 0
		 nextY += 1
	       }
	       Pair( nextX, nextY )
	     },
	     function )
  }

  /**
   * Iterates over all spaces within the board between the two given points.
   * Calls the given function for each of those spaces.
   * @param topLeftX The top left X point
   * @param topLeftY The top left Y point
   * @param bottomRightX The bottom right x point
   * @param bottomRightY The bottom right y point
   * @param function The function to call for the current x and y position
   */
  def foreachBox( topLeftX: Int, topLeftY: Int, bottomRightX: Int, bottomRightY: Int, function: ( Int, Int ) => Unit ) {
    foreach( topLeftX,
	     topLeftY,
	     NodeBoard.makeDoneWhenBeyond( bottomRightX, bottomRightY ),
	     ( currentX: Int, currentY: Int ) => {
	       var nextX = currentX + 1
	       var nextY = currentY

	       if ( nextX > bottomRightX ) {
		 nextX = topLeftX
		 nextY += 1
	       }
	       Pair( nextX, nextY )
	     },
	     function )
  }

  /**
   * Iterates over the board, starting at the given X and Y position.
   * Gives the current X and Y to the given function.  Goes to the end
   * of the board.
   * @param startX The starting X coordinate
   * @param startY The starting Y coordinate
   * @param function The function to call for each x and y
   */
  def foreach( startX: Int, startY: Int, function: ( Int, Int ) => Unit ) {
    foreach( startX,
	     startY,
	     width - 1,
	     height - 1,
	     function )
  }

  /**
   * Iterates over the position of the board, with the
   * given top left position, and the width and height of the given node
   * @param x The top left x position
   * @param y The top left y position
   * @param node The node to get the width and height of
   * @param function The function to call for each x and y
   */
  def foreachBox( x: Int, 
		  y: Int, 
		  node: Node[ T, U ], 
		  function: ( Int, Int ) => Unit ) {
    foreachBox( x, y,
	        x + node.width,
	        y + node.height,
	        function )
  }

  /**
   * Iterates over each position in the board, calling the given function
   * for each X, Y position.
   * @param function The function to call for each X and Y
   */
  def foreach( function: ( Int, Int ) => Unit ) {
    foreach( 0, 0, function )
  }

  /**
   * Gets the coordinates of the bottom right corner of the given node
   * @pre The node is on the board
   * @param node The node
   * @return The bottom right coordinate
   * @throws NoSuchNodeException If the given node is not on the board
   */
  def getBottomRightCorner( node: Node[ T, U ] ) = {
    verifyNode( node )

    val topLeft = corners( node )
    Pair( topLeft._1 + node.width,
	  topLeft._2 + node.height )
  }

  /**
   * Gets all the nodes within the given box.  Note that nodes might not
   * be fully contained within the area will still be returned.  I.e. if
   * only a corner of a node is within the area, that node is still returned
   * @param topX The top left corner of the box's x position
   * @param topY The top left corner of the box's y position
   * @param bottomX The bottom right corner of the box's X position
   * @param bottomY The bottom right cornder of the box's Y position
   * @return All the nodes within the given area
   */
  def getNodes( topX: Int,
	        topY: Int,
	        bottomX: Int,
	        bottomY: Int ) = {
    var retval: Set[ Node[ T, U ] ] = Set()

    foreachBox( topX,
	        topY,
	        bottomX,
	        bottomY,
	        ( x: Int, y: Int ) => {
		  val item = board( x )( y )
		  if ( item.isDefined && !retval.contains( item.get ) ) {
		    retval += item.get
		  }
		} )
    retval
  }

  /**
   * Determines if the given point is in bounds
   * @param x The x position
   * @param y The y position
   * @return true if it's in bounds, else false
   */
  def inBounds( x: Int, y: Int ) = {
    ( x >= 0 &&
      x < width &&
      y >= 0 &&
      y < height )
  }

  /**
   * Determines if the given node can be placed at the given position.
   * This means that there are no other nodes at the given location, and the
   * position is in bounds.
   * 
   * @param node The node
   * @param x The x position
   * @param y The y position
   * @param selfCount If this node counts against itself; i.e. the only node
   * blocking the way is itself, as in moving an existing node.  Set this to true
   * if it does count against itself
   * @return true if the node is placable, else false
   */
  def nodePlaceable( node: Node[ T, U ], 
		     x: Int, 
		     y: Int, 
		    selfCount: Boolean ): Boolean = {
    val bottomX = x + node.width
    val bottomY = y + node.height
    var retval = false

    if ( inBounds( x, y ) && 
	 inBounds( bottomX, bottomY ) ) {
	   val nodes = getNodes( x, y,
				 bottomX,
				 bottomY )
	   if ( selfCount ) {
	     retval = nodes.isEmpty
	   } else {
	     retval = ( nodes.isEmpty ||
		        ( nodes.size == 1 &&
			  nodes.forall( _.eq( node ) ) ) )
	   }
	 }
    retval
  }

  /**
   * Determines if the given node can be placed at the given position.
   * This is equivalent to <code>nodePlaceable( node, x, y, true )</code>
   * @param node The node to try to place
   * @param x The x position of the node's top left corner
   * @param y The y position of the node's top left corner
   * @return true if the node is placable, else false
   */
  def nodePlaceable( node: Node[ T, U ], x: Int, y: Int ): Boolean =
    nodePlaceable( node, x, y, true )

  /**
   * Validates that the node can be placed at the given position
   * @param node The node to place
   * @param x The x position
   * @param y The y position
   * @throws NodeUnplacableException If placing the node would result in a collision
   */
  def validPlacement( node: Node[ T, U ], x: Int, y: Int ) {
    if ( !nodePlaceable( node, x, y ) ) {
      throw new NodeUnplacableException( "Given node cannot be placed at " +
				         "(" + x + ", " + y + ")" )
    }
  }

  /**
   * Fills the given space in the board with the given node.
   * @param topX The x position of the top left corner of the node
   * @param topY The y position of the top left corner of the node
   * @param node The node to place
   */
  def fillBoard( topX: Int, topY: Int, node: Node[ T, U ] ) {
    val toPlace = Some( node )
    foreachBox( topX,
	        topY,
	        node,
	        ( currentX: Int, currentY: Int ) => {
		  board( currentX )( currentY ) = toPlace
		} )
  }

  /**
   * Fills the given space of the board with the given item.
   * @param topX The x position of the top left corner
   * @param topY The y position of the top left corner
   * @param bottomX The x position of the bottom corner
   * @param bottomY The y position of the bottom corner
   * @param item The item to fill the area with
   */
  def fillBoard( topX: Int, 
		 topY: Int, 
		 bottomX: Int, 
		 bottomY: Int, 
		 item: Option[ Node[ T, U ] ] ) {
    foreachBox( topX, 
	        topY, 
	        bottomX, 
	        bottomY,
	       ( currentX: Int, currentY: Int ) => {
		 board( currentX )( currentY ) = item
	       } )
  }

  /**
   * Places the given node at the given board location.
   * @param node The node to place
   * @param x The x position
   * @param y The y position
   * @throws NodeUnplacableException If placing the node would
   * result in a collision
   */
  def placeNode( node: Node[ T, U ], x: Int, y: Int ) {
    validPlacement( node, x, y )
    fillBoard( x, y, node )
    corners += Pair( node, Pair( x, y ) )
  }

  /**
   * Verifies that the given node is on the board
   * @throws NoSuchNodeException If the given node is not on the board
   */
  def verifyNode( node: Node[ T, U ] ) {
    if ( !corners.contains( node ) ) {
      throw new NoSuchNodeException( "The given node is not on the board" )
    }
  }

  /**
   * Determines if the given node can be moved to the given place
   * @param node The node to move
   * @param x The top left corner of where to put it
   * @param y The top left corner of where to put it
   * @return true if it can be moved there, else false
   * @throws NoSuchNodeException If the given node isn't on the board
   */
  def nodeMoveable( node: Node[ T, U ], x: Int, y: Int ) = {
    verifyNode( node )
    nodePlaceable( node, x, y, false )
  }

  /**
   * Blanks the area of the board consumed by the given node
   * @throws NoSuchNodeException If the given node is not on the board
   */
  def blankArea( node: Node[ T, U ] ) {
    verifyNode( node )
    val ( topX, topY ) = corners( node )
    val ( bottomX, bottomY ) = getBottomRightCorner( node )
    fillBoard( topX, topY,
	       bottomX, bottomY,
	       None )
  }

  /**
   * Removes the given node from the board.  Disconnects the node from
   * everything else before removing it.
   * @param node The node to remove
   * @throws NoSuchNodeException If the given node isn't on the board
   */
  def removeNode( node: Node[ T, U ] ) {
    verifyNode( node )
    node.disconnect()
    blankArea( node )
    corners -= node
  }

  /**
   * Moves the given node to the given position.
   * @param node The node to move
   * @param x The top left corner where to put the node
   * @param y The top left corner where to put the node
   * @throws NoSuchNodeException If the given node isn't on the board
   * @throws NodeUnplacableException If the given space isn't open to move to
   */
  def moveNode( node: Node[ T, U ], x: Int, y: Int ) {
    if ( !nodeMoveable( node, x, y ) ) {
      throw new NodeUnplacableException( "Space (" + x + ", " + y + ") isn't" +
					 " open" )
    }
    
    // empty out the area the node was previously at
    blankArea( node )
    
    // fill in the new area
    placeNode( node, x, y )
  }

  /**
   * Gets the node at the given position
   * @param x The x coordinate
   * @param y The y coordinate
   * @return The node here, or None if there isn't one
   */
  def getNode( x: Int, y: Int ) =
    board( x )( y )

  /**
   * Synonym for <code>getCorner</code>
   * @param node The node to get the top left corner of
   * @return The top left corner of the node
   */
  def getTopLeftCorner( node: Node[ T, U ] ) =
    getCorner( node )

  /**
   * Gets the top left corner of the given node.
   * @param node The node to get the top left corner of
   * @return A pair containing the (x,y) coordinate pair for
   * the top left corner of the given node, or None if the
   * given node is not within.
   */
  def getCorner( node: Node[ T, U ] ) =
    corners.get( node )

  /**
   * Gets all nodes on the board.  This is much more
   * efficient than a call to <code>getNodes</code> with the whole
   * board specified.
   * @return all nodes in the board
   */
  def getNodes() =
    corners.keys.toList
}
