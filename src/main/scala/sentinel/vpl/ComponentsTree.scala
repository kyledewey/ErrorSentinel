/*
 * ComponentsTree.scala
 */

package sentinel.vpl

import sentinel.model._
import javax.swing._
import javax.swing.event._
import javax.swing.tree._
import java.awt._
import java.awt.event._

/**
 * Holds constants for <code>ComponentsTree</code>
 * @author Kyle Dewey
 */
object ComponentsTreeHelpers {
  val VIEWPORT_WIDTH_INCREMENT = 100
  val VIEWPORT_HEIGHT_INCREMENT = 100
}

/**
 * A tree that holds components that can be created into nodes.
 * Note that the leaf nodes MUST be of type <code>ComponentKey[ T, U ]</code>
 * @param root The root node of the tree
 * @param gui The gui holding everything together
 * @author Kyle Dewey
 */
class ComponentsTree[ T <: AnyRef, U ]( val gui: ComponentsTreeGUIView[ T, U ], 
				        val model: ComponentsTreeModel )
extends JTree( model ) with TreeSelectionListener with MouseInputListener {
  // begin instance variables
  private var lastKeySelected: Option[ ComponentKey[ T, U ] ] = None
  private var draggingOut = false
  // end instance variables

  // begin constructor
  addTreeSelectionListener( this )
  addMouseListener( this )
  addMouseMotionListener( this )
  //setEditable( true )
  // end constructor

  /**
   * Creates a tree with the given node as the root node.
   * @param gui The gui that we are associated with
   * @param root The root node
   */
  def this( gui: ComponentsTreeGUIView[ T, U ],
	    root: DefaultMutableTreeNode ) = 
    this( gui, new ComponentsTreeModel( root ) )

  /**
   * Gets the preferred size of the viewport
   * For some reason, the defaults are overconservative.
   * Merely adds <code>VIEWPORT_WIDTH_INCREMENT</code> and
   * <code>VIEWPORT_HEIGHT_INCREMENT</code> to the original size.
   * @return The preferred scrollable viewpoer size.
   */
  override def getPreferredScrollableViewportSize() = {
    val original = super.getPreferredScrollableViewportSize
    original.setSize( original.getWidth + 
		       ComponentsTreeHelpers.VIEWPORT_WIDTH_INCREMENT,
		      original.getHeight + 
		       ComponentsTreeHelpers.VIEWPORT_HEIGHT_INCREMENT )
    original
  }

  /**
   * Returns a key from the given tree path.
   * @param path The tree path
   * @return The tree key at this path
   */
  def getKey( path: TreePath ) = {
    val last = path.getLastPathComponent
    var retval: Option[ ComponentKey[ T, U ] ] = None
    
    if ( last.isInstanceOf[ DefaultMutableTreeNode ] ) {
      val asNode = last.asInstanceOf[ DefaultMutableTreeNode ]
      if ( !asNode.getAllowsChildren && // must be a leaf
	   asNode.getUserObject.isInstanceOf[ ComponentKey[ _, _ ] ] ) {
	retval = Some( asNode.getUserObject.asInstanceOf[ ComponentKey[ T, U ] ] )
      } 
    }

    retval
  }
      
  /**
   * Records when a selection occurs on the tree.
   * If the user clicks on a leaf, then we want to show a
   * description of the leaf.
   * @param event The selection event correlating to the click
   */
  def valueChanged( event: TreeSelectionEvent ) {
    lastKeySelected = getKey( event.getPath )

    if( lastKeySelected.isDefined ) {
      gui.showDescription( lastKeySelected.get )
    }
  }

  /**
   * Gets the tree key at the given X, Y position
   * @param x The x position
   * @param y The y position
   * @return The tree key here
   */
  def getKey( x: Int, y: Int ): Option[ ComponentKey[ T, U ] ] = {
    val path = getPathForLocation( x, y )
    if ( path != null ) {
      getKey( path )
    } else {
      None
    }
  }

  /**
   * Gets the key for the given mouse event.
   * @param event The mouse event
   * @return The key for this event, or None if there isn't one
   */
  def getKey( event: MouseEvent ): Option[ ComponentKey[ T, U ] ] =
    getKey( event.getX, event.getY )

  /**
   * Tells the GUI that we are dragging in the tree.
   * @param event The mouse event that correlates to the drag
   */
  def mouseDragged( event: MouseEvent ) {
    gui.treeDragging( event )
  }

  /**
   * Triggered when this is clicked.  Doesn't do anything.
   * @param event The event associated with the click
   */
  def mouseClicked( event: MouseEvent ) {}

  /**
   * Triggered when the mouse enters this.
   * Doesn't do anything.
   * @param event The event associated with the enter
   */
  def mouseEntered( event: MouseEvent ) {}
  
  /**
   * Tells the GUI that we have exited the tree.
   * @param event The event associated with leaving the tree
   */
  def mouseExited( event: MouseEvent ) {
    if ( draggingOut ) {
      gui.nodeDraggingOut( lastKeySelected.get.makeNode )
    }
  }

  /**
   * If we clicked on a key, then this will start the process
   * of dragging out a component.
   * @param event The event correlating to the drag
   */
  def mousePressed( event: MouseEvent ) {
    // note that this is called after valueChanged
    val keyHere = getKey( event )
    if ( lastKeySelected.isDefined &&
	 keyHere.isDefined &&
	 lastKeySelected.get == keyHere.get ) {
      draggingOut = true
    }
  }

  /**
   * Tells the GUI that the mouse has been released.
   * If we were dragging a node, this will cause it to be placed.
   * @param event The event correlating to the release
   */
  def mouseReleased( event: MouseEvent ) {
    draggingOut = false
    gui.nodeReleased( event )
  }

  /**
   * Triggered when the mouse is moved in this component.
   * Doesn't actually do anything.
   * @param event The event correlating to the move
   */
  def mouseMoved( event: MouseEvent ) {}
}

/**
 * Tree that is specific to Sentinel.
 * @param gui The gui that is holding everything together
 * @param root The root of the tree
 * @author Kyle Dewey
 */
class SentinelTree( gui: ComponentsTreeGUIView[ InstanceFactory[ _ ], Param ],
		    root: DefaultMutableTreeNode )
extends ComponentsTree[ InstanceFactory[ _ ], Param ]( gui, root ) {}

/**
 * Contains routines to make SentinelTrees with different types of roots
 * @author Kyle Dewey
 */
object SentinelTree {
  import java.io._

  /**
   * Creates a tree with the given gui and root
   * @param gui The gui
   * @param root The root for the tree
   * @return A sentinel tree made from the given information
   */
  def apply( gui: ComponentsTreeGUIView[ InstanceFactory[ _ ], Param ],
	     root: DefaultMutableTreeNode ): SentinelTree = {
    new SentinelTree( gui, root )
  }

  /**
   * Creates a tree, using the given base directory for components.
   * It will recursively look for components in the base directory and
   * include them.
   * @param gui The gui holding everything together
   * @param baseDir The base directory
   * @return A sentinel tree holding all the components in the given directory
   */
  def apply( gui: ComponentsTreeGUIView[ InstanceFactory[ _ ], Param ],
	     baseDir: String ): SentinelTree = {
    apply( gui, 
	   SentinelTreeHelpers.makeTreeNodeFromDir( baseDir ) )
  }

  /**
   * Creates a tree, using the given project file.
   * @param gui The gui holding everything together
   * @param project The project file.  Assumes that this is in XML
   * @return A sentinel tree holding all the components from the file
   */
  def apply( gui: ComponentsTreeGUIView[ InstanceFactory[ _ ], Param ],
	     project: File ): SentinelTree = {
    apply( gui,
	   SentinelTreeHelpers.makeTreeNodeFromProject( project ) )
  }
}
