/*
 * VisualNode.scala
 */

package sentinel.vpl

import sentinel.model._
import javax.swing._
import java.awt._

/**
 * Holds constants for visual nodes.
 * @author Kyle Dewey
 */
object VisualNode {
  // size of input/outputs
  val INPUT_WIDTH = 5
  val INPUT_HEIGHT = 5

  // default line color
  val DEFAULT_LINE_COLOR = Color.BLACK

  // amount to subtract from the width and height
  // for drawing text
  val DRAW_TEXT_WIDTH_ADD = 0
  val DRAW_TEXT_HEIGHT_ADD = -INPUT_HEIGHT

  // text colors
  val NORMAL_TEXT_COLOR = Color.BLACK
  val TRUNCATED_TEXT_COLOR = Color.BLUE

  /**
   * Given a node, a board, and a label, it will create a visual version
   * of the node.  Note that the given label may not be used depending
   * on the type of the node
   * @param node The node
   * @param board The board that the node is on
   * @param label A label for the node.  Possibly won't be used.
   * @return A specific visual node for the given needs
   */
  def apply[ T <: AnyRef, U ]( node: Node[ T, U ],
			       board: NodeBoard[ T, U ],
			       label: String ): VisualNode[ T, U ] = {
    import sentinel.model.replacer._

    node match {
      case d: SentinelDebuggingNode =>
	new DebuggingNode( d, board.asInstanceOf[ NodeBoard[ InstanceFactory[ _ ], Param ] ] )
      case s: SentinelNode =>
	new VisualNode[ T, U ]( s, board, s.factory.name )
      case _ =>
	new VisualNode[ T, U ]( node, board, label )
    }
  }

  /**
   * Draws a filled rectangle on the given graphics, using the given
   * top left and bottom right corners of the rectangle.
   * @param points The top left and bottom right corners of the rectangle
   * @param color The color to draw the rectangle
   * @param graphics The graphics object to draw on
   */
  def fillRect( points: ((Int, Int), (Int, Int)),
	        color: Color,
	        graphics: Graphics ) {
    val oldColor = graphics.getColor
    graphics.setColor( color )
    graphics.fillRect( points._1._1,
		       points._1._2,
		       points._2._1 - points._1._1,
		       points._2._2 - points._1._2 )
    graphics.setColor( oldColor )
  }

  /**
   * Draws a string of the given maximum width of the given color.
   * @param string The string to draw
   * @param x The x position of the string
   * @param y The y position of the string
   * @param maxWidth the maximum width of the string
   * @param normalColor Color of the string if the string didn't need
   * to be truncated
   * @param truncColor Color of the string if it needed to be truncated
   * @param graphics The graphics object to draw with
   */
  def drawString( string: String,
		  x: Int,
		  y: Int,
		  maxWidth: Int,
		  normalColor: Color,
		  truncColor: Color,
		  graphics: Graphics ) {
    val oldColor = graphics.getColor
    val truncated = truncateString( string,
				    graphics,
				    maxWidth )
    val newColor = 
      if ( truncated.length == string.length ) 
	normalColor 
      else 
	truncColor

    graphics.setColor( newColor )
    graphics.drawString( truncated,
			 x, y )
    graphics.setColor( oldColor )
  }

  /**
   * Like <code>drawString</code>, but it uses <code>NORMAL_TEXT_COLOR</code>
   * and <code>TRUNCATED_TEXT_COLOR</code>.
   * @param string The string to draw
   * @param x The x position to draw at
   * @param y The y position to draw at
   * @param maxWidth The maximum width of the string
   * @param graphics The graphics object to draw with
   */
  def drawString( string: String,
		  x: Int,
		  y: Int,
		  maxWidth: Int,
		  graphics: Graphics ) {
    drawString( string,
	        x, y,
	        maxWidth,
	        NORMAL_TEXT_COLOR,
	        TRUNCATED_TEXT_COLOR,
	        graphics )
  }

  /**
   * Determines if the given point is within the rectangle dictated
   * by the given coordinates.
   * @param point The point to determine
   * @param topLeft The top left corner of the rectangle
   * @param bottomRight The bottom right corner of the rectangle
   * @return true if the point is within the rectange, else false
   */
  def pointWithin( point: (Int, Int), 
		   topLeft: (Int, Int),
		   bottomRight: (Int, Int) ) = {
    point._1 >= topLeft._1 &&
    point._1 <= bottomRight._1 &&
    point._2 >= topLeft._2 &&
    point._2 <= bottomRight._2
  }

  /**
   * Gets the Euclidean distance between two points
   * @param point1 The first point
   * @param point2 The second point
   * @return The distance between the two
   */
  def distance( point1: (Int, Int), point2: (Int, Int) ): Double =
    distance( point1._1,
	      point1._2,
	      point2._1,
	      point2._2 )
  
  /**
   * Gets the Euclidean distance between two points
   * @param px The first point's x
   * @param py The first point's y
   * @param qx The second point's x
   * @param qy The second point's y
   * @return The distance between the two
   */
  def distance( px: Int, py: Int, qx: Int, qy: Int ): Double =
    Math.sqrt( Math.pow( px - qx, 2 ) + 
	       Math.pow( py - qy, 2 ) )
  
  /**
   * Gets the width of the given box
   * @param box The box
   * @return The width of the box
   */
  def width( box: ((Int, Int), (Int, Int)) ) =
    box._2._1 - box._1._1

  /**
   * Gets the height of the given box
   * @param box The box
   * @return The height of the box
   */
  def height( box: ((Int, Int), (Int, Int)) ) =
    box._2._2 - box._1._2

  /**
   * Given a box, returns a point for each edge.  The point is in the middle
   * of the edge.
   * @param box The box to get the mid points of
   * @return The middle point for each of the edges
   */
  def edgeMidPoints( box: ((Int, Int), (Int, Int)) ) = {
    val middleX = ( width( box ) / 2 ) + box._1._1
    val middleY = ( height( box ) / 2 ) + box._1._2
    
    Seq( (middleX, box._1._2),
	 (middleX, box._2._2),
	 (box._1._1, middleY),
	 (box._2._1, middleY) )
  }
  
  /**
   * Given two boxes, gets the two points that should be used as
   * end points between a line between them.  It picks the closest edge.
   * @param box1 The first box
   * @param box2 The second box
   * @return The points that should be used as edge points for a line
   */
  def lineEndPoints( box1: ((Int, Int), (Int, Int)),
		     box2: ((Int, Int), (Int, Int)) ) = {
    var firstPoint: (Int, Int) = null
    var secondPoint: (Int, Int) = null
    var retvalDistance = 0.0

    edgeMidPoints( box1 ).foreach( ( box1Edge: (Int, Int) ) => {
      edgeMidPoints( box2 ).foreach( ( box2Edge: (Int, Int) ) => {
	val currentDistance = distance( box1Edge, box2Edge )

	if ( firstPoint == null ||
	     currentDistance < retvalDistance ) {
	  retvalDistance = currentDistance
	  firstPoint = box1Edge
	  secondPoint = box2Edge
	}
      })
    })

    (firstPoint, secondPoint)
  }
    
  /**
   * Draws a line between the two boxes.  The line is put at
   * the center of the edge closest to the other
   * @param box1 The first box
   * @param box2 The second box
   * @param color The color to draw the line
   * @param graphics The graphics object to draw with
   */
  def drawLineBoxes( box1: ((Int, Int), (Int, Int)),
	             box2: ((Int, Int), (Int, Int)),
	             color: Color,
	             graphics: Graphics ) {
    val ( firstPoint,
	  secondPoint ) = lineEndPoints( box1, box2 )
    drawLinePoints( firstPoint,
		    secondPoint,
		    color,
 		    graphics )
  }

  /**
   * Like <code>drawLine</code>, but it uses DEFAULT_LINE_COLOR.
   * @param box1 The first box
   * @param box2 The second box
   * @param graphics The graphics object to draw with
   */
  def drawLineBoxes( box1: ((Int, Int), (Int, Int)),
	             box2: ((Int, Int), (Int, Int)),
	             graphics: Graphics ) {
    drawLineBoxes( box1,
		   box2,
 		   DEFAULT_LINE_COLOR,
		   graphics )
  }

  /**
   * Draws a line between the two given points.
   * @param point1 The first point
   * @param point2 The second point
   * @param color The color of the line
   * @param graphics The graphics object to draw with
   */
  def drawLinePoints( point1: (Int, Int),
	              point2: (Int, Int),
	              color: Color,
	              graphics: Graphics ) {
    val oldColor = graphics.getColor
    graphics.setColor( color )
    graphics.drawLine( point1._1,
		       point1._2,
		       point2._1,
		       point2._2 )
    graphics.setColor( oldColor )
  }

  /**
   * Draws a line between the two given points.
   * Uses <code>DEFAULT_LINE_COLOR</code>
   * @param point1 The first point
   * @param point2 The second point
   * @param graphics The graphics object to draw with
   */
  def drawLinePoints( point1: (Int, Int),
	              point2: (Int, Int),
	              graphics: Graphics ) {
    drawLinePoints( point1,
		    point2,
		    DEFAULT_LINE_COLOR,
		    graphics )
  }

  /**
   * Given a string and a graphics object that is to draw the string,
   * it returns the width of the given string.
   * @param string The string to get the width of
   * @param graphics The graphics object that is to draw the string
   * @return The width of the string, in pixels, if it were to be drawn
   * by the given graphics object
   */
  def width( string: String, graphics: Graphics ) =
    graphics.getFontMetrics
            .getStringBounds( string, graphics )
            .getWidth
  
  /**
   * Given a string, a graphics object used to draw the string,
   * and a target width, it will return a truncated string that is no
   * larger than the target width.
   * @param string The string to truncate
   * @param graphics The graphics object that is to draw the string
   * @param widthTarget The target width of the string
   * @return A truncated string that will fit in the given width
   */
  final def truncateString( string: String, 
			    graphics: Graphics, 
			    widthTarget: Int ): String = {
    if ( string == "" ||
	 width( string, graphics ) <= widthTarget ) 
      string
    else
      truncateString( string.substring( 0, string.length - 1 ),
		      graphics,
		      widthTarget )
  }

  /**
   * Orders all the given nodes by their X positions on the board.
   * Assumes that all nodes are on the same board (results will be given
   * if not, but they won't be valid.)
   * @param nodes The nodes to sort by X position
   * @return The sorted nodes
   */
  def sortNodes[ T <: AnyRef, U ]( nodes: Seq[ VisualNode[ T, U ] ] ) =
    nodes.toList.sortWith( _.topLeftCorner._1 < _.topLeftCorner._1 ).toSeq
}

/**
 * Represents a node in the GUI.
 * Such nodes are based on actual Nodes.  However, this class contains
 * code for specifically manipulating nodes in the GUI.  Note that there is
 * no code for painting connections.  Connections must be painted last, and
 * there is no good way of enforcing this from the perspective of a single node.
 * @param node The node that this is supposed to be painting
 * @param board The board that the node is on
 * @param label A text label for the node
 * @author Kyle Dewey
 */
class VisualNode[ T <: AnyRef, U ]( val node: Node[ T, U ],
			            val board: NodeBoard[ T, U ],
				    private var _label: String ) {
  /**
   * Gets the value of the label
   * @return The value of the label
   */
  def label = 
    _label
    
  /**
   * Sets the value of the label
   * @param newLabel The new value of the label
   */
  def label_=( newLabel: String ): Unit =
    _label = newLabel

  /**
   * Gets the top left corner of this node.
   * @return The top left corner of this node
   */
  def topLeftCorner() =
    board.getCorner( node ).get

  /**
   * Gets the width of this node.
   * @return The width of this node
   */
  def width() = node.width

  /**
   * Gets the height of this node
   * @return The height of this node
   */
  def height() = node.height

  /**
   * Gets the bottom right corner of this node.
   * @return The bottom right corner of this node
   */
  def bottomRightCorner() = {
    val topLeft = topLeftCorner
    (topLeft._1 + width,
     topLeft._2 + height)
  }
  
  /**
   * Gets the amount that each input should be staggered on the x axis for
   * this node.
   * @return The amount of stagger for each input on the x axis
   */
  def stagger() = {
    val numInputs = node.numInputs
    Math.ceil( ( width - ( VisualNode.INPUT_WIDTH * numInputs ) ) / 
	      ( numInputs + 1 ).asInstanceOf[ Double ] ).asInstanceOf[ Int ]
  }

  /**
   * Gets where the output box should be for this node.
   * The output box is represented as a top left corner
   * and a bottom right corner.
   * @return The position of the output box
   */
  def outputBoxPosition() = {
    val bottomRight = bottomRightCorner
    ((bottomRight._1 - VisualNode.INPUT_WIDTH,
      bottomRight._2 - VisualNode.INPUT_HEIGHT),
     bottomRight)
  }

  /**
   * Returns the positions of each of the input boxes for this node.
   * Each position is represented as a pair of coordinate pairs.
   * @return The positions of the input boxes
   */
  def inputBoxPositions() = {
    val topLeft = topLeftCorner
    var retval: Seq[ ((Int, Int), (Int, Int)) ] = Seq()
    var currentX = stagger

    while( currentX < width ) {
      retval ++= Seq( ((topLeft._1 + currentX,
			topLeft._2 ),
		       (topLeft._1 + currentX + VisualNode.INPUT_WIDTH,
			topLeft._2 + VisualNode.INPUT_HEIGHT)) )
      currentX += stagger + VisualNode.INPUT_WIDTH
    }

    retval
  }

  /**
   * Paints out the main rectangle of this node.
   * @param graphics The graphics object to paint with
   */
  def paintNodeBase( graphics: Graphics ) {
    VisualNode.fillRect( (topLeftCorner,
		          bottomRightCorner),
			 node.color,
			 graphics )
  }

  /**
   * Paints all the input lines of this node
   * @param graphics The graphics object to paint with
   */
  def paintInputLines( graphics: Graphics ) {
    var currentInput = 0
    inputBoxPositions.foreach( 
      ( box: ((Int, Int), (Int, Int)) ) => {
	VisualNode.fillRect( box,
			     node.inputColor( currentInput ),
			     graphics )
	currentInput += 1
      })
  }

  /**
   * Paints the output line of this node
   * @param graphics The graphics object to paint with
   */
  def paintOutputLine( graphics: Graphics ) {
    VisualNode.fillRect( outputBoxPosition,
			 node.outputColor,
			 graphics )
  }

  /**
   * Paints all the input and output lines of this node
   * @param graphics The graphics object to paint with
   */
  def paintLines( graphics: Graphics ) {
    paintInputLines( graphics )
    paintOutputLine( graphics )
  }

  /**
   * Paints out this node.  Note that this only refers to the
   * node itself, not to anything that it is connected to.
   * @param graphics The graphics object to paint with
   */
  def paintNode( graphics: Graphics ) {
    paintNodeBase( graphics )
    paintLines( graphics )
    paintText( graphics )
  }

  /**
   * Paints the text label of a node.
   * @param graphics The graphics object to paint with
   */
  def paintText( graphics: Graphics ) {
    val ( xPos,
	  yPos ) = topLeftCorner
    VisualNode.drawString( label,
			   xPos + VisualNode.DRAW_TEXT_WIDTH_ADD,
			   yPos + height + VisualNode.DRAW_TEXT_HEIGHT_ADD,
			   width - VisualNode.DRAW_TEXT_WIDTH_ADD,
			   graphics )
  }

  /**
   * Given a node and an x and y coordinate, it returns whether or
   * not the output line was selected.  Assumes that the x and y
   * coordinates are within the node
   * @pre The given x and y coordinate are in the node
   * @param x The x coordinate of selection
   * @param y The y coordinate of selection
   * @return true if the output line was selected, else false
   */
  def outputLineSelected( x: Int, y: Int ) = {
    val ( topLeft,
	  bottomRight ) = outputBoxPosition
    VisualNode.pointWithin( (x, y),
			    topLeft,
			    bottomRight )
  }

  /**
   * Given an x, y coordinate, returns the name of the
   * input that was selected.
   * @pre The x, y coordinate is within this node
   * @param x The x coordinate
   * @param y The y coordinate
   * @return The name of the input that was selected, or None
   * if no inputs were selected
   */
  def selectedInput( x: Int, y: Int ) = {
    val point = (x, y)
    val inputPositions = inputBoxPositions.toArray
    var inputNumber = 0
    var retval: Option[ String ] = None
    var done = false

    while( inputNumber < inputPositions.length &&
	   !done ) {
      val ( topLeftInput,
	    bottomRightInput ) = inputPositions( inputNumber )
      if ( VisualNode.pointWithin( point,
				   topLeftInput,
				   bottomRightInput ) ) {
	retval = Some( node.inputNumberToName( inputNumber ) )
	done = true
      }
      inputNumber += 1
    }

    retval
  }

  /**
   * Given an x, y coordinate, returns which line was selected.
   * @pre The x, y coordinate is within this node
   * @param x The x coordinate
   * @param y The y coordinate
   * @return The name of the input that was selected, or OUTPUT_LINE if
   * the output was selected, or None if no lines were selected
   */
  def selectedLine( x: Int, y: Int ) = 
    if ( outputLineSelected( x, y ) ) {
      Some( Node.OUTPUT_LINE )
    } else {
      selectedInput( x, y )
    }
}

/**
 * A special visual node just for debugging.
 * @param debugNode The node that underlies this one
 * @param board The board that the node is on
 * @author Kyle Dewey
 */
class DebuggingNode( val debugNode: SentinelDebuggingNode,
		     board: NodeBoard[ InstanceFactory[ _ ], Param ] )
extends VisualNode( debugNode, board, debugNode.variable.variable ) {
  /**
   * Resets the value of this node to a default value.
   * This value is <code>SentinelDebuggingNode.DEFAULT_VARIABLE_VALUE</code>
   */
  def reset(): Unit =
    label = SentinelDebuggingNode.DEFAULT_VARIABLE_VALUE

  /**
   * Gets the value of the label.
   * @return The value of the label
   */
  override def label =
    debugNode.variable.variable

  /**
   * Sets the value of the label
   * @param newLabel The new value of the label
   */
  override def label_=( newLabel: String ): Unit =
    debugNode.variable.variable = newLabel
}

