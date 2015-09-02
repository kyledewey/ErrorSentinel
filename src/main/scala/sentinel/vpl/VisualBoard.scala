/*
 * VisualBoard.scala
 */

package sentinel.vpl

import sentinel.model._
import javax.swing._
import javax.swing.event._
import java.awt._
import java.awt.event._

/**
 * Holds constants for VisualBoard.
 * @author Kyle Dewey
 */
object VisualBoard {
  // background color 
  val BACKGROUND_COLOR = Color.WHITE

  // border color
  val BORDER_COLOR = Color.BLACK

  // stroke used for moving nodes
  val MOVING_NODE_COLOR = Color.BLACK
  val MOVING_NODE_STROKE = new BasicStroke( 1.0f, 
					    BasicStroke.CAP_BUTT,
					    BasicStroke.JOIN_MITER,
					    5.0f,
					    Array( 5.0f ),
					    0.0f )

  // the user attempted to connect two inputs
  val INPUT_TO_INPUT_CONNECTION_ERROR = "Cannot connect inputs to each other"

  // the user attempted to connect two outputs
  val OUTPUT_TO_OUTPUT_CONNECTION_ERROR = "Cannot connect outputs to each other"

  val DEFAULT_NODE_LABEL = "NO LABEL"

  /**
   * Converts the given mouse event to a pair containing
   * the x and y coordinates of the point within
   * @param event The mouse event
   * @return The x and y coordinates in a pair
   */
  def toPair( event: MouseEvent ) =
    (event.getX, event.getY )

  /**
   * For each node in the given model, it will make a visual node representing
   * the node.  Returns nodes as a mapping of the original nodes to their new
   * visual nodes.
   * @param board The board containing nodes
   * @param creator Given a node from the board, it will create a visual node
   * based on it
   * @return a mapping of the original nodes to visual nodes
   */
  def makeVisualNodes[ T <: AnyRef, U ]( board: NodeBoard[ T, U ],
					 creator: ( Node[ T, U ] ) => VisualNode[ T, U ] ): 
  Map[ Node[ T, U ], VisualNode[ T, U ] ] =
    Map() ++ board.getNodes.map( ( node: Node[ T, U ] ) =>
      (node, creator( node )) )
  
  /**
   * Creates a visual node from a regular node, using <code>DEFAULT_NODE_LABEL</code>.
   * @param node The regular node
   * @param board The board the node is on
   * @return A visual node based on this node
   */
  def makeVisualNode[ T <: AnyRef, U ]( node: Node[ T, U ],
				        board: NodeBoard[ T, U ] ) = {
    VisualNode( node, 
	        board,
	        DEFAULT_NODE_LABEL )
  }
	       
  /**
   * Like <code>makeVisualNodes</code>, but for all nodes, it will try a label
   * of <code>DEFAULT_NODE_LABEL</code>.
   * @param board Board containing nodes
   * @return A mapping of the original nodes to visual nodes
   */
  def makeVisualNodes[ T <: AnyRef, U ]( board: NodeBoard[ T, U ] ): 
  Map[ Node[ T, U ], VisualNode[ T, U ] ] =
    makeVisualNodes( board,
		     makeVisualNode( _, board ) )
  
  /**
   * Gets the node that would need to be updated
   * if the given line on the given node were to be disconnected.
   * @param node The node
   * @param line The line on the node that is to be disconnected
   * @return The node that would have to be updated if the given line
   * were disconnected.  If there are none, it returns None.
   */
  def nodeToUpdateIfDisconnect[ T <: AnyRef, U ]( node: Node[ T, U ],
						  line: String ) = {
    var retval: Option[ Node[ T, U ] ] = None

    if ( line.eq( Node.OUTPUT_LINE ) ) {
      if ( node.output.isDefined ) {
	retval = Some( node.output.get._2 )
      }
    } else {
      retval = Some( node )
    }

    retval
  }
}

/**
 * Represents an internal state of the visual board.
 * Depending on what the user has done in the past,
 * different actions are applicable.
 * Putting all of this in one class gets cumbersome.
 * @param board The visual board that we are associated with.
 * @author Kyle Dewey
 */
abstract class VisualBoardState[ T <: AnyRef, U ]
( val board: VisualBoard[ T, U ] ) extends MouseInputListener {
  /**
   * Paints components that are specific to this state.
   * @param graphics The graphics object to paint with
   */
  def paintComponent( graphics: Graphics ): Unit
}

/**
 * Default state for the visual board.
 * @param board The visual board that we are assciated with
 * @author Kyle Dewey
 */
class DefaultVisualBoardState[ T <: AnyRef, U ]
( board: VisualBoard[ T, U ] ) extends VisualBoardState[ T, U ]( board ) {
  /**
   * Performs a specific action to clicks.
   * This will first look to see if we tried to debug something.
   * If not, it will show the description for a node that was clicked (if
   * applicable), then attempt to remove a node (if applicable)
   * @param event Event correlating to the click
   */
  override def mouseClicked( event: MouseEvent ) {
    if ( SwingUtilities.isLeftMouseButton( event ) ) {
      if ( event.getClickCount == 1 ) {
	singleLeftClick( event )
      } else if ( event.getClickCount == 2 ) {
	doubleLeftClick( event ) 
      }
    } else if ( SwingUtilities.isRightMouseButton( event ) ) {
      if ( event.getClickCount == 1 ) {
	singleRightClick( event )
      }
    }
  }

  /**
   * Performs a specific action to entering.
   * Informs the main gui that we have entered the board.
   * @param event Event correlating to the enter action
   */
  override def mouseEntered( event: MouseEvent ) {
    board.gui.mouseEnteredBoard()
  }

  /**
   * Performs a specific action to exiting.
   * Informs the main gui that we exited the board
   * @param event Event correlating to the exit action
   */
  override def mouseExited( event: MouseEvent ) {
    board.gui.mouseExitedBoard()
  }

  /**
   * Whenever a single left click is performed, this will be called.
   * @param event The event that correlates to a single left click
   */
  def singleLeftClick( event: MouseEvent ) {
    board.attemptShowNodeSelected( event )
  }

  /**
   * Whenever a double left click is performed, this will be called.
   * @param event The event that correlates to a double left click
   */
  def doubleLeftClick( event: MouseEvent ) {
    board.attemptDebug( event ) 
  }

  /**
   * Whener a single right click is performed, this will be called.
   * @param event The event that correlates to the single right click
   */
  def singleRightClick( event: MouseEvent ) {
    if ( !board.attemptDisconnect( event ) ) {
      board.attemptRemove( event )
    }
  }

  /**
   * Performs a specific action upon pressing the mouse.
   * By default, this will conditionally move us into the input selected
   * state or the node selected state.
   * @param event The event correlating to the press
   */
  override def mousePressed( event: MouseEvent ) {
    if ( SwingUtilities.isLeftMouseButton( event ) ) {
      val selectedNode = board.getSelectedNode( event )
      
      if ( selectedNode.isDefined ) {
	val selectedLine = board.visualNode( selectedNode.get )
	                        .selectedLine( event.getX,
					       event.getY )
	if ( selectedLine.isDefined ) {
	  linePressed( selectedNode.get,
		       selectedLine.get,
		       event )
	} else {
	  nodePressed( selectedNode.get,
		       event )
	}
      } else {
	emptyAreaPressed( event )
      }
    }
  }

  /**
   * Called when the user presses on an empty area.
   * @pre The press occurred with the left mouse button
   * @param event The event that correlates to the press
   */
  def emptyAreaPressed( event: MouseEvent ) {
    board.changeState( new AreaSelectedState( board,
					      event.getX,
					      event.getY ) )
    board.repaint()
  }

  /**
   * Called when the user presses on a node but not a line on the node.
   * @pre The press occurred with the left mouse button
   * @param node The node that has been pressed
   * @param event The event that correlates to the press
   */
  def nodePressed( node: Node[ T, U ], event: MouseEvent ) {
    val visualNode = board.visualNode( node )
    
    if ( visualNode.selectedLine( event.getX,
				  event.getY ).isEmpty ) {
      val nodePos = visualNode.topLeftCorner
      board.changeState( new NodeSelectedState( board,
					        node,
					        (event.getX - nodePos._1,
						 event.getY - nodePos._2) ) )
      board.repaint()
    }
  }

  /**
   * Called when the user presses on a node and an input on that node
   * @pre The press occurred with the left mouse button
   * @param node The node that has been pressed
   * @param line The line on the node that has been pressed
   * @param event The event that correlates to the mouse being pressed
   */
  def linePressed( node: Node[ T, U ],
		   line: String,
		   event: MouseEvent ) {
    board.changeState( new InputSelectedState( board,
					       node,
					       line,
					       event.getX,
					       event.getY ) )
    board.repaint()
  }

  /**
   * Performs a specific action for the mouse being released.
   * By default, this does nothing.
   * @param event Event correlating to the mouse being released
   */
  override def mouseReleased( event: MouseEvent ) {}

  /**
   * Performs a specific action for the mouse being dragged.
   * By default, this does nothing.
   * @param event The event that correlates to the mouse being dragged
   */
  override def mouseDragged( event: MouseEvent ) {}

  /**
   * Performs a specific action for the mouse being moved.
   * By default, this does nothing
   * @param event The event that correlates to the mouse being moved
   */
  override def mouseMoved( event: MouseEvent ) {}

  /**
   * Performs drawing that is specific to this state
   * By default, this clears the board, paints all nodes, and
   * paints all connections
   * @param graphics The graphics object to draw with
   */
  override def paintComponent( graphics: Graphics ) {
    board.clear( graphics )
    board.paintNodes( graphics )
    board.paintConnections( graphics )
  }
}

/**
 * Holds constants for NodeSelectedState
 * @author Kyle Dewey
 */
object NodeSelectedState {
  val MOVING_NODE_COLOR = Color.BLACK
  val MOVING_NODE_STROKE = new BasicStroke( 1.0f, 
					    BasicStroke.CAP_BUTT,
					    BasicStroke.JOIN_MITER,
					    5.0f,
					    Array( 5.0f ),
					    0.0f )
  /**
   * Paints a box with a given color and stroke.
   * @param x The top left x coordinate
   * @param y The top left y coordinate
   * @param width The width of the box
   * @param height The height of the box
   * @param color The color of the box
   * @param stroke The stroke to use for the box
   * @param graphics The graphics object to draw with
   */
  def paintBox( x: Int,
                y: Int,
                width: Int,
                height: Int,
                color: Color,
                stroke: Stroke,
                graphics: Graphics ) {
    val as2D = graphics.asInstanceOf[ Graphics2D ]
    val oldStroke = as2D.getStroke
    val oldColor = as2D.getColor

    as2D.setColor( color )
    as2D.setStroke( stroke )
    as2D.drawRect( x, y,
                   width, height )
    as2D.setStroke( oldStroke )
    as2D.setColor( oldColor )
  }

  /**
   * Paints a box using <code>MOVING_NODE_COLOR</code> and
   * <code>MOVING_NODE_STROKE</code>
   * @param x The top left x coordinate
   * @param y The top left y coordinate
   * @param width The width of the box
   * @param height The height of the box
   * @param graphics The graphics object to draw with
   */
  def paintBox( x: Int,
	        y: Int,
	        width: Int,
	        height: Int,
	        graphics: Graphics ) {
    paintBox( x, y,
	      width,
	      height,
	      MOVING_NODE_COLOR,
	      MOVING_NODE_STROKE,
	      graphics )
  }
}

/**
 * Represents a state where the user has selected a node, and he is dragging
 * it to move the node.
 * @param board The board that we are associated with
 * @param selectedNode The node that has been selected
 * @param nodePositionX The origin of the node
 * @param nodePositionY The origin of the node
 * @param originDifference The difference between the origin of the selected node
 * and where the user clicked.
 * @author Kyle Dewey
 */
class NodeSelectedState[ T <: AnyRef, U ]
( board: VisualBoard[ T, U ], 
  private val node: Node[ T, U ], 
  private var nodePositionX: Int,
  private var nodePositionY: Int,
  private val originDifference: (Int, Int) ) 
extends DefaultVisualBoardState[ T, U ]( board ) {
  /**
   * Determines the position of the node using it's current position on
   * the board.
   * @pre The given node is on the given board
   * @param board The board that we are associated with
   * @param node The node that has been selected
   * @param originDifference The difference between the origin of
   * the selected node and where the user clicked
   */
  def this( board: VisualBoard[ T, U ],
	    node: Node[ T, U ], 
	    originDifference: (Int, Int) ) = 
    this( board,
	  node,
	  board.model.getTopLeftCorner( node ).get._1,
	  board.model.getTopLeftCorner( node ).get._2,
	  originDifference )

  /**
   * Indicates that the mouse has been dragged.
   * Updates the position of the node outline
   * @param event Event that correlates to the drag
   */
  override def mouseDragged( event: MouseEvent ) {
    nodePositionX = event.getX - originDifference._1
    nodePositionY = event.getY - originDifference._2
    board.repaint()
  }

  /**
   * Indicates that the mouse has been released.
   * Attempts to move the selected node to the given position.
   * @param event Event that correlates to the mouse release
   */
  override def mouseReleased( event: MouseEvent ) {
    try {
      board.model.moveNode( node,
			    event.getX - originDifference._1,
			    event.getY - originDifference._2 )
    } catch {
      case e: NodeUnplacableException => board.showError( e )
    }
    board.changeState( new DefaultVisualBoardState( board ) )
    board.repaint()
  }

  /**
   * Draws the outline of the moving node
   * @param graphics The graphics object to draw with
   */
  override def paintComponent( graphics: Graphics ) {
    super.paintComponent( graphics )
    NodeSelectedState.paintBox( nodePositionX,
			        nodePositionY,
			        node.width,
			        node.height,
			        graphics )
  }
}
    
/**
 * Represents a state where a user has selected an input, and he is dragging
 * it to make a connection.
 * @param board The board that we are associated with
 * @param node The node that has been selected
 * @param line The line (input/output) that has been selected
 * @param linePositionX The position where the user has clicked
 * @param linePositionY The position where the user has clicked
 * @author Kyle Dewey
 */
class InputSelectedState[ T <: AnyRef, U ]
( board: VisualBoard[ T, U ],
  private val node: Node[ T, U ],
  private val line: String,
  private var linePositionX: Int,
  private var linePositionY: Int )
extends DefaultVisualBoardState[ T, U ]( board ) {
  // begin instance variables
  private val startingLinePositionX = linePositionX
  private val startingLinePositionY = linePositionY
  private val startingLinePosition = (startingLinePositionX,
				      startingLinePositionY)
  private var dontDraw = false
  // end instance variables

  /**
   * Indicates that the mouse has been dragged.
   * This means that a connection is starting to be made between nodes
   * @param event The mouse event that correlates to the event
   */
  override def mouseDragged( event: MouseEvent ) {
    super.mouseDragged( event )
    linePositionX = event.getX
    linePositionY = event.getY
    board.repaint()
  }

  /**
   * To be called when the user releases the mouse while the user
   * is attempting to make a connection.  If we were released over
   * a connection, then this will try to make the connection.  If not,
   * then this won't do anything.
   * @param event The mouse event correlating to the release
   */
  def connectIfPossible( event: MouseEvent ) {
    val nodeHere = board.getSelectedNode( event )

    if ( nodeHere.isDefined ) {
      val lineHere = board.visualNode( nodeHere.get ).selectedLine( event.getX,
								    event.getY )
      if ( lineHere.isDefined ) {
	board.connect( node,
		       line,
		       nodeHere.get,
		       lineHere.get )
      }
    }
  }

  /**
   * Indicates that the mouse has been released
   * This means that we should try to make a connection
   * @param event The event that correlates to the mouse being released
   */
  override def mouseReleased( event: MouseEvent ) {
    super.mouseReleased( event )
    connectIfPossible( event )
    dontDraw = true
    board.repaint()
  }

  /**
   * Shows a connection being made
   * @param graphics The graphics object to draw with
   */
  override def paintComponent( graphics: Graphics ) {
    super.paintComponent( graphics )
    if ( !dontDraw ) {
      VisualNode.drawLinePoints( (linePositionX, 
				  linePositionY),
				 (startingLinePositionX,
				  startingLinePositionY),
				 graphics )
    }
  }
}

/**
 * Holds constants and static routines for AreaSelectedState.
 * @author Kyle Dewey
 */
object AreaSelectedState {
  /**
   * Converts all elements of a set to the given type
   * @param set The set to convert
   * @return The converted set
   */
  def convertSet[ T ]( set: Set[ _ ] ) =
    Set[ T ]() ++ set.map( _.asInstanceOf[ T ] )

  /**
   * Converts all the given elements of a set to SentinelNodes.
   * @param set The set to convert
   * @return The converted set
   */
  def convertSetToSentinel( set: Set[ _ ] ) =
    convertSet[ SentinelNode ]( set )

  /**
   * Gets all nodes in the board in the given rectangle.
   * @param board The board that contains nodes
   * @param rect The rectangle holding a portion of the board.
   * @return A set of Nodes contained within the area
   */
  def getNodes[ T <: AnyRef, U ]( board: VisualBoard[ T, U ],
				  rect: Rectangle ) = {
    board.model.getNodes( rect.startX,
			  rect.startY,
			  rect.endX,
			  rect.endY )
  }

  /**
   * Determines if the given x value is in the board.
   * If it isn't, it will get the closest X that is.
   * @param x The x value
   * @param board The board
   * @return An x value that is guarenteed to be in the board.
   */
  def xInBoard[ T <: AnyRef, U ]( x: Int, board: VisualBoard[ T, U ] ) = 
    if ( x < 0 ) {
      0
    } else if ( x >= board.model.width ) {
      board.model.width - 1
    } else {
      x
    }
  
  /**
   * Determines if the given y value is in the board.
   * If it isn't, it will get the closest Y that is.
   * @param y The y value
   * @param board The board
   * @return A y value that is guarenteed to be in the board
   */
  def yInBoard[ T <: AnyRef, U ]( y: Int, board: VisualBoard[ T, U ] ) =
    if ( y < 0 ) {
      0
    } else if ( y >= board.model.height ) {
      board.model.height - 1
    } else {
      y
    }

  /**
   * Makes sure that the given rectangle fits with the given board.
   * If it doesn't, it will make a new rectangle that's as close
   * to the old one as possible.
   * @param rect The rectangle to check
   * @param board The board that the rectangle's supposed to go into
   * @return A rectangle that is guarenteed to be in the board.
   * Note that if <code>rect</code> already fits, then it will be
   * returned as-is.
   */
  def fitRect[ T <: AnyRef, U ]( rect: Rectangle, board: VisualBoard[ T, U ] ) = {
    val newRect = new Rectangle( xInBoard( rect.startX, board ),
				 yInBoard( rect.startY, board ),
				 xInBoard( rect.endX, board ),
				 yInBoard( rect.endY, board ) )
    if ( newRect == rect ) {
      rect
    } else {
      newRect
    }
  }

  /**
   * Like <code>getNodes</code>, but it returns sentinel nodes.
   * @param board The board that contains nodes
   * @param rect Rectangle holding a portion of the board
   * @return A set of SentinelNodes contained in the area
   */
  def getSentinelNodes[ T <: AnyRef, U ]( board: VisualBoard[ T, U ],
					  rect: Rectangle ) = {
    convertSetToSentinel( getNodes( board, 
				    fitRect( rect, board ) ) )
  }
}

/**
 * Represents a state where the user is attempting to select an entire region.
 * This is for function creation.
 * @param board The board that we are associated with
 * @param startPointX Where we started selecting
 * @param startPointY Where we started selecting
 * @author Kyle Dewey
 */
class AreaSelectedState[ T <: AnyRef, U ]
( board: VisualBoard[ T, U ],
  startPointX: Int,
  startPointY: Int )
extends DefaultVisualBoardState[ T, U ]( board ) {
  // begin instance variables
  private val rect = new SelectionRectangle( startPointX, 
					     startPointY )
  // end instance variables
  
  /**
   * Indicates that the mouse has been dragged.
   * This updates where the end of the box is
   * @param event The event that correlates to the drag
   */
  override def mouseDragged( event: MouseEvent ) {
    super.mouseDragged( event )
    rect.adjustRectangle( event.getX,
			  event.getY )
    board.repaint()
  }

  /**
   * Given a set of sentinel nodes, returns a sorted seq.
   * The sort is based on <code>sortNodes</code> in VisualBoard.
   * @param nodes The set of nodes
   * @return The sorted sequence of nodes
   */
  def sortNodes( nodes: Set[ SentinelNode ] ) =
    board.sortNodes( nodes.toSeq
		          .map( _.asInstanceOf[ Node[ T, U ] ] ) )
         .map( _.asInstanceOf[ SentinelNode ] )
  
  /**
   * Creates a PreFunction from the given nodes.
   * @param nodes The nodes to make the function from
   * @return A GUI function based on the given nodes
   * @param BadOutputException If there were not exactly one possible output
   * @param InputNotConnectedException If there is an unreachable node from
   */
  def makePreFunction( nodes: Set[ SentinelNode ] ) =
    new PreFunction( nodes,
		     sortNodes( nodes ) )
  
  /**
   * Makes a function panel for the given nodes.
   * @param nodes The nodes to make the panel for
   * @param BadOutputException If there were not exactly one possible output
   * @param InputNotConnectedException If there is an unreachable node from
   * the output.
   */
  def functionPanel( nodes: Set[ SentinelNode ] ) {
    val frame = new JFrame( "Create Function" )
    frame.add( new GUIFunction( makePreFunction( nodes ),
			        board.gui.languageManager ) )
    frame.setDefaultCloseOperation( WindowConstants.DISPOSE_ON_CLOSE )
    frame.pack()
    frame.setVisible( true )
  }

  /**
   * Indicates that the mouse has been released.
   * This means that we should try to make a function from the
   * nodes that were within the box.
   * @param event The event that correlates to the mouse being released
   */
  override def mouseReleased( event: MouseEvent ) {
    super.mouseReleased( event )
    try {
      functionPanel( AreaSelectedState.getSentinelNodes( board, rect ) )
    } catch {
      case BadOutputException( message, numOutputs ) => {
	if ( numOutputs != 0 ) {
	  board.showError( message )
	}
      }
      case e: InputNotConnectedException => board.showError( e )
    }
    board.changeState( new DefaultVisualBoardState( board ) )
    board.repaint()
  }

  /**
   * Draws the outline of the area being selected on the screen
   * @param graphics The graphics object to draw with
   */
  override def paintComponent( graphics: Graphics ) {
    super.paintComponent( graphics )
    NodeSelectedState.paintBox( rect.startX,
			        rect.startY,
			        rect.endX - rect.startX,
			        rect.endY - rect.startY,
			        graphics )
  }
}

import sentinel.utils.interactive.ErrorShower

/**
 * Represents the canvas on which the user moves and connects nodes.
 * Note that the entire GUI is contained in three portions: the board (this),
 * a description panel for selected components, and a panel for putting
 * components on the board.
 * @param model The underlying board
 * @param gui The main gui that holds everything
 * @author Kyle Dewey
 */
class VisualBoard[ T <: AnyRef, U ]( val model: NodeBoard[ T, U ],
				     val gui: VisualBoardGUIView[ T, U ] ) 
extends JComponent with ErrorShower with MouseInputListener {
  // maps regular nodes to their visual version
  var visualNode: Map[ Node[ T, U ], VisualNode[ T, U ] ] = 
    VisualBoard.makeVisualNodes( model )

  // current state that we are in
  private var state: VisualBoardState[ T, U ] = 
    new DefaultVisualBoardState( this )

  setBackground( VisualBoard.BACKGROUND_COLOR )
  addMouseListener( this )
  addMouseMotionListener( this )
  repaint()

  /**
   * Boxes the given seq of nodes to their visual equivalents.
   * @param nodes The nodes to box
   * @return The boxed nodes
   */
  def toVisualNodes( nodes: Seq[ Node[ T, U ] ] ) =
    nodes.map( visualNode( _ ) )

  /**
   * Boxes the given seq of visual nodes to their normal
   * equivalents.
   * @param nodes The nodes to convert
   * @return The boxed nodes
   */
  def toRegularNodes( nodes: Seq[ VisualNode[ T, U ] ] ) =
    nodes.map( _.node )

  /**
   * Sorts the given set of nodes.
   * The actual sort uses VisualNode's <code>sortNodes</code> routine.
   * @param nodes The nodes to sort
   * @return The sorted nodes
   */
  def sortNodes( nodes: Seq[ Node[ T, U ] ] ) =
    toRegularNodes( VisualNode.sortNodes( toVisualNodes( nodes ) ) )

  /**
   * Changes the state of this board to the given state.
   * @param state The new state to use
   */
  def changeState( state: VisualBoardState[ T, U ] ) {
    this.state = state
  }

  /**
   * Gets the preferred size of the component.
   * This is the same as the width and height of the model
   * @return The width and height of the model
   */
  override def getPreferredSize() =
    new Dimension( model.width, model.height )

  /**
   * Gets the minimum size of the component
   * This is the same as the preferred size
   * @return The minimum size of the component (the preferred size)
   */
  override def getMinimumSize() =
    getPreferredSize

  /**
   * Paints all the input connections for the given node
   * @param node The node to paint the inputs of
   * @param graphics The graphics object to paint with
   */
  def paintInputConnections( node: Node[ T, U ],
			     graphics: Graphics ) {
    lazy val inputBoxPositions = visualNode( node ).inputBoxPositions.toArray
    var currentInput = 0

    node.inputNames.foreach( ( name: String ) => {
      node.input( currentInput ).foreach( ( connectedTo: Node[ T, U ] ) => {
	VisualNode.drawLineBoxes( inputBoxPositions( currentInput ),
				  visualNode( connectedTo ).outputBoxPosition,
				  graphics )
      })
      currentInput += 1
    })
  }

  /**
   * Paints all connections between all nodes
   * @param graphics The graphics object to paint with
   */
  def paintConnections( graphics: Graphics ) {
    model.getNodes.foreach( paintInputConnections( _, graphics ) )
  }
      

  /**
   * Draws the border
   * @param graphics The graphics object to draw with
   */
  def drawBorder( graphics: Graphics ) {
    graphics.setColor( VisualBoard.BORDER_COLOR )
    graphics.drawRect( 0, 0, model.width - 1, model.height - 1 )
  }

  /**
   * Clears out the screen
   * @param graphics The graphics object to draw with
   */
  def clear( graphics: Graphics ) {
    graphics.setColor( VisualBoard.BACKGROUND_COLOR )
    graphics.fillRect( 0, 0, model.width, model.height )
    drawBorder( graphics )
  }

  /**
   * Paints all of the nodes on the board
   * @param graphics The graphics object to paint with
   */
  def paintNodes( graphics: Graphics ) {
    visualNode.values.foreach( _.paintNode( graphics ) )
  }

  /**
   * Returns that this is opaque
   * @return true
   */
  override def isOpaque() = true

  /**
   * Paints the board.
   * @param graphics The graphics object to paint with
   */
  override def paintComponent( graphics: Graphics ) {
    super.paintComponent( graphics )
    state.paintComponent( graphics )
  }

  /**
   * Given an x and y coordinate, returns the node that was selected, or None
   * if no nodes were selected
   * @param x The x coordinate
   * @param y The y coordinate
   * @return the node that was selected, or None if none were selected
   */
  def getSelectedNode( x: Int, y: Int ): Option[ Node[ T, U ] ] =
    model.getNode( x, y )

  /**
   * Given a mouse event, gets the node that was at the x and y coordinates
   * of the node
   * @param event The mouse event
   * @return The node that was selected, or None if none were selected
   */
  def getSelectedNode( event: MouseEvent ): Option[ Node[ T, U ] ] =
    getSelectedNode( event.getX, event.getY )

  /**
   * Attempts to show that a node has been selected.
   * Does not assume that this is the case
   * @pre The user has performed a single left click
   * @param event The mouse event that correlates to a click
   */
  def attemptShowNodeSelected( event: MouseEvent ) {
    val nodeHere = getSelectedNode( event )
    
    if ( nodeHere.isDefined ) {
      gui.nodeSelected( nodeHere.get )
    }
  }

  /**
   * Removes the given node from the board
   * @param node The node to remove
   * @throws NoSuchNodeException If the given node isn't on the board
   */
  def removeNode( node: Node[ T, U ] ) {
    val child = node.output
    model.removeNode( node )
    visualNode -= node

    if ( child.isDefined ) {
      updateDebuggers( child.get._2 )
    }
    repaint()
  }

  /**
   * Attempts to remove a node.  When one right clicks on a node, it is removed.
   * Does not assume that this has happened.  Note that this will remove a node
   * if the input/output line were selected.
   * @pre The user has right clicked
   * @param event The mouse event that correlates to a click
   */
  def attemptRemove( event: MouseEvent ) {
    val nodeHere = getSelectedNode( event )
    
    if ( nodeHere.isDefined ) {
      removeNode( nodeHere.get )
    }
  }

  /**
   * Disconnects the given input/output line on the given node
   * @param node The node to disconnect
   * @param line The line to disconnect
   */
  def disconnect( node: Node[ T, U ], line: String ) {
    val toUpdate = VisualBoard.nodeToUpdateIfDisconnect( node, line )
    node.disconnect( line )
    if ( toUpdate.isDefined ) {
      updateDebuggers( toUpdate.get )
    }
  }
    
  /**
   * Attempts to disconnect connections.  When one right clicks on
   * an input line or an output line, all connections on the given
   * line are disconnected.  Does not assume that this is the case
   * @pre The user has performed a single right click
   * @param event The mouse event that correlates to a click
   * @return true if the user clicked on an input/output for disconnect, else
   * false.  Note that this will return true if an unconnected line was selected
   * for disconnection
   */
  def attemptDisconnect( event: MouseEvent )  = {
    var retval = false
    val nodeHere = getSelectedNode( event )
    
    if ( nodeHere.isDefined ) {
      val lineHere = visualNode( nodeHere.get ).selectedLine( event.getX,
							      event.getY )
      if ( lineHere.isDefined ) {
	retval = true
	disconnect( nodeHere.get,
		    lineHere.get )
	repaint()
      }
    }

    retval
  }

  /**
   * If the user double clicked on a debugging node, then
   * we want to change the value of that node.
   * @pre The user has performed a double left click
   * @param event The mouse event that describes what was clicked
   * @return true if the user double clicked on a debugging node,
   * else false
   */
  def attemptDebug( event: MouseEvent ) = {
    var retval = false

    val nodeHere = getSelectedNode( event )
      
    if ( nodeHere.isDefined && 
	 visualNode( nodeHere.get ).isInstanceOf[ DebuggingNode ] ) {
      val asDebug = visualNode( nodeHere.get ).asInstanceOf[ DebuggingNode ]
      val newLabel = JOptionPane.showInputDialog( this,
						  "Input debugging value.",
						  asDebug.label )
      retval = true
      if ( newLabel != null && 
	   newLabel != asDebug.label ) {
	asDebug.label = newLabel
	updateDebuggers( nodeHere.get )
	repaint()
      }
    }

    retval
  }
      
  /**
   * When the user clicks on the board, we want to display information about the
   * given item that has been clicked.
   * @param event The mouse event that describes where we clicked
   */
  def mouseClicked( event: MouseEvent ) {
    state.mouseClicked( event )
  }

  /**
   * Occurs when the mouse enters the board.
   * @param event The event correlating to the mouse entering the board
   */
  def mouseEntered( event: MouseEvent ) {
    state.mouseEntered( event )
  }

  /**
   * Occurs when the mouse exits the board.
   * @param event The event correlating to the mouse exiting the board
   */
  def mouseExited( event: MouseEvent ) {
    state.mouseExited( event )
  }

  /**
   * Tells us that we should show the siloette of a node moving.
   * Doesn't assume the node is on the board
   * @param node The node to show a siloette of
   * @param x The x position of the node
   * @param y The y position of the node
   */
  def showNodeOutline( node: Node[ T, U ], x: Int, y: Int ) {
    changeState( new NodeSelectedState( this,
				        node,
				        x, y,
				        (x, y) ) )
    repaint()
  }

  /**
   * Tells us to forget the outline of a node.
   */
  def forgetNodeOutline() {
    changeState( new DefaultVisualBoardState( this ) )
    repaint()
  }
    
  /**
   * When the user clicks and drags on a node, we want to
   * move an outline of the given node to show where the node would go.
   * @param event Event describing the move
   */
  def mousePressed( event: MouseEvent ) { 
    state.mousePressed( event )
  }

  /**
   * Connects the given node to the other node on the other node's input.
   * @param node1 The node to connect to the other node
   * @param node2 The node that the first connects to
   * @param line The input line on node2 that node1 connects to
   * @return true if the connection was successful, else false
   */
  protected def connect( node1: Node[ T, U ],
			 node2: Node[ T, U ],
			 line: String ): Boolean = {
    var connectionMade = false

    try {
      node2.connect( node1, line )
      updateDebuggers( node1 )
      connectionMade = true
    } catch {
      // thrown when a connection was never attempted
      case e: InvalidNodeConnectionException => showError( e )
    }

    connectionMade
  }

  /**
   * Attempts to connect the two nodes.
   * If a connection isn't possible, then the user will be shown a message
   * why.
   * @param node1 The first node in the connection
   * @param node1Line The line on node1 to use for the connection
   * @param node2 The second node in the connection
   * @param node2Line The line on node2 to use for the connection
   * @return true if a connection was made, else false
   */
  def connect( node1: Node[ T, U ],
	       node1Line: String,
	       node2: Node[ T, U ],
	       node2Line: String ): Boolean = {
    var connectionMade = false

    if ( node1Line.eq( Node.OUTPUT_LINE ) &&
	 node2Line.eq( Node.OUTPUT_LINE ) ) {
      showError( VisualBoard.OUTPUT_TO_OUTPUT_CONNECTION_ERROR )
    } else if ( !node1Line.eq( Node.OUTPUT_LINE ) &&
	        !node2Line.eq( Node.OUTPUT_LINE ) ) {
      showError( VisualBoard.INPUT_TO_INPUT_CONNECTION_ERROR )
    } else if ( node1Line.eq( Node.OUTPUT_LINE ) ) {
      connectionMade = connect( node1, node2, node2Line )
    } else {
      connectionMade = connect( node2, node1, node1Line )
    }


    connectionMade
  }

  /**
   * Resets the debugging values of all the child nodes of the
   * given node.
   * @param node The node to reset the debuggers of
   */
  def resetChildDebuggers( node: Node[ T, U ] ) {
    def getOutput( myNode: Node[ T, U ] ) =
      if ( myNode.output.isEmpty ) {
	None
      } else {
	Some( myNode.output.get._2 )
      }
    
    def resetChildDebuggers( myNode: Option[ Node[ T, U ] ] ) {
      if ( myNode.isDefined ) {
	val visual = visualNode( myNode.get )
	if ( visual.isInstanceOf[ DebuggingNode ] ) {
	  visual.asInstanceOf[ DebuggingNode ].reset()
	}
	resetChildDebuggers( getOutput( myNode.get ) )
      }
    }

    resetChildDebuggers( Some( node ) )
  }

  /**
   * Given a node, it will look for debuggers connected to
   * the node whoose values may have changed.  It will
   * update them to show the change.  
   * @pre The node is already on the board
   * @param node The node that has been placed
   * @throws InputException If an input to a node was invalid
   */
  def updateDebuggers( node: Node[ T, U ] ) {
    try {
      node.lastChild.returnValue()
    } catch {
      case e: InputException[ _, _ ] => {
	e.node.disconnectInput( e.input )
	showError( e )
	resetChildDebuggers( e.node.asInstanceOf[ Node[ T, U ] ] )
      }
      case e: Exception => showError( e )
    }
  }

  /**
   * Attempts to place the given node on the board at the given position.
   * @param node The node to place
   * @param x The x position
   * @param y The y position
   */
  def placeNode( node: Node[ T, U ], x: Int, y: Int ) {
    try {
      model.placeNode( node, x, y )
      val visNode = VisualBoard.makeVisualNode( node, model )
      visualNode += (node -> visNode)
      repaint()
    } catch {
      case e: NodeUnplacableException => showError( e )
    }
  }

  /**
   * When the user releases, we want to move the node to this position
   * @param event Event describing the move
   */
  def mouseReleased( event: MouseEvent ) { 
    state.mouseReleased( event )
  }

  /**
   * Notes when the mouse is being dragged.
   * When the user is moving a node, we want to move the outline of a node
   * around with the mouse.
   * @param event Event describing the drag
   */
  def mouseDragged( event: MouseEvent ) {
    state.mouseDragged( event )
  }

  /**
   * Indicates that the mouse has moved.
   * @param event Event descriving the move
   */
  def mouseMoved( event: MouseEvent ) {
    state.mouseMoved( event )
  }
}

