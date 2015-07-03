/*
 * SentinelVPL.scala
 *
 * Version:
 *     $Id: SentinelVPL.scala,v 1.5 2011/06/20 22:19:56 kyledewey Exp $
 *
 * Revisions:
 *      $Log: SentinelVPL.scala,v $
 *      Revision 1.5  2011/06/20 22:19:56  kyledewey
 *      Changed layouts to BoxLayouts.  GUI no longer "jumps".
 *      Added an exit menu routine.
 *
 *      Revision 1.4  2011/06/01 04:05:11  kyledewey
 *      Now parses in the base language before parsing in
 *      anything else.
 *
 *      Revision 1.3  2011/04/10 04:06:46  kyledewey
 *      Implemented the loading in of projects and
 *      the creation of new language files.
 *
 *      Revision 1.2  2011/04/08 02:31:41  kyledewey
 *      Added support for loading project files.
 *
 * 
 */

package sentinel.vpl

import java.io._
import java.awt._
import java.awt.event._
import javax.swing._
import javax.swing.tree._
import sentinel.model._

/**
 * Holds constants and static routines for SentinelVPL.
 * @author Kyle Dewey
 */
object SentinelVPL {
  val TITLE = "Sentinel Programming Language"
  val NEW_LANG_TEXT = "New/Load Language"
  val LOAD_PROJ_TEXT = "Load Project File"
  val EXIT_TEXT = "Exit"
}

/**
 * The main GUI for the VPL.  Holds everything together.
 * @param boardCreator Function that creates the visual board.
 * The visual board needs the whole GUI as a parameter, so it
 * passes <code>this</code> to it
 * @param descriptionPanelCreator Like <code>boardCreator</code>,
 * but with the descrption panel
 * @param componentsCreator Like <code>boardCreator</code>, but with
 * the components panel.
 * @author Kyle Dewey
 */
class SentinelVPL[ T <: AnyRef, U ]( boardCreator: (VisualBoardGUIView[ T, U ]) => VisualBoard[ T, U ],
				     descriptionPanelCreator: (SentinelVPL[ T, U ]) => DescriptionPanel,
				     componentsCreator: (ComponentsTreeGUIView[ T, U ]) => ComponentsTree[ T, U ] )  
extends JFrame( SentinelVPL.TITLE ) with CompleteVPL[ T, U ] with ActionListener {
  import sentinel.project._
  import SentinelVPL._

  LanguageReader.readBaseLanguage()
  // begin instance variables
  private val actions: Map[ String, () => Unit ] = 
    Map( LOAD_PROJ_TEXT -> loadProjectAction,
         NEW_LANG_TEXT -> newLanguageAction,
	 EXIT_TEXT -> exitAction )
  private var nodeDragging: Option[ Node[ T, U ] ] = None
  private var mouseInBoard = false
  val board = boardCreator( this )
  val descPanel = descriptionPanelCreator( this )
  val components = componentsCreator( this )
  val scrollPane = new JScrollPane( components )
  val topPanel = new JPanel()
  val languageManager = new LanguageManager( components.model )
  // end instance variables

  // begin constructor
  setJMenuBar( makeMenu )
  setSize( board.model.width,
	   board.model.height )
  topPanel.setLayout( new BoxLayout( topPanel, BoxLayout.X_AXIS ) )
  topPanel.add( board )
  topPanel.add( scrollPane )
  topPanel.setComponentOrientation( ComponentOrientation.LEFT_TO_RIGHT )

  setLayout( new BorderLayout() )
  add( topPanel, BorderLayout.NORTH )
  add( descPanel, BorderLayout.SOUTH )
  add( topPanel )
  // end constructor

  /**
   * Shows a description of the given describable thing
   * @param item The describable item
   */
  def showDescription( item: Describable ) {
    descPanel.showDescription( item )
  }

  /**
   * Says that the given node has been selected
   * @param node The node that has been selected
   */
  def nodeSelected( node: Node[ T, U ] ) {
    showDescription( node )
  }

  /**
   * Says that the given node is being dragged away
   * from the tree that held it.
   * @param node The node that is being moved
   */
  def nodeDraggingOut( node: Node[ T, U ] ) {
    nodeDragging = Some( node )
  }

  /**
   * Tells us where we are dragging to, relative to the components tree.
   * TO BE CALLED ONLY BY THE TREE.
   * @param event The mouse event correlating to the drag
   */
  def treeDragging( event: MouseEvent ) {
    if ( nodeDragging.isDefined &&
	 mouseInBoard ) {
      val location = SwingUtilities.convertPoint( components,
						  event.getX,
						  event.getY,
						  board )
      board.showNodeOutline( nodeDragging.get,
			     location.x,
			     location.y )
    }
  }

  /**
   * Says that the node that we were dragging out has been released.
   * TO BE CALLED ONLY BY THE TREE.
   * @param event The mouse event on the board that correlates to the release
   */
  def nodeReleased( event: MouseEvent ) {
    if ( nodeDragging.isDefined &&
	 mouseInBoard ) {
      val location = SwingUtilities.convertPoint( components,
						  event.getX,
						  event.getY,
						  board )
      board.placeNode( nodeDragging.get,
		       location.x,
		       location.y )
    }
    nodeDragging = None
    board.forgetNodeOutline()
  }

  /**
   * Says that the mouse is in the board.
   */
  def mouseEnteredBoard() {
    mouseInBoard = true
  }

  /**
   * Says that the mouse exited the board.
   */
  def mouseExitedBoard() {
    mouseInBoard = false
  }

  /**
   * Makes a menu item with the given text.
   * Registers <code>this</code> as an ActionListener.
   * @param text The text for the item
   * @return The menu item
   */
  protected def makeMenuItem( text: String ) = {
    val retval = new JMenuItem( text )
    retval.addActionListener( this )
    retval
  }

  /**
   * Makes the menu for this component.
   * @return The menu to use for this component
   */
  protected def makeMenu() = {
    val menuBar = new JMenuBar()
    val file = new JMenu( "File" )
    file.add( makeMenuItem( NEW_LANG_TEXT ) )
    file.add( makeMenuItem( LOAD_PROJ_TEXT ) )
    file.add( makeMenuItem( EXIT_TEXT ) )
    menuBar.add( file )
    menuBar
  }
  
  /**
   * Creates a LoadedLanguage that is associated with this VPL.
   * @param node The node that is associated with this language.
   * @param filePath Path to the language file associated with this language
   */
  def makeLanguage( node: DefaultMutableTreeNode ) =
    new SentinelLanguage( components.model, node )

  /**
   * Prompts the user to create a new, empty language.
   */
  def newLanguageAction() {
    val saver = new JFileChooser( "." )
    val action = saver.showSaveDialog( this )
    
    if ( action == JFileChooser.APPROVE_OPTION ) {
      val file = saver.getSelectedFile
      val node = SentinelTreeHelpers.makeLanguageNode( file )
      languageManager.addLanguage( makeLanguage( node ) )
      components.model.insertNodeIntoRoot( node )
    }
  }

  /**
   * Opens a user dialog for loading in a language file.
   */
  def loadProjectAction() {
    val chooser = new JFileChooser( "." )
    val action = chooser.showOpenDialog( this )

    if ( action == JFileChooser.APPROVE_OPTION ) {
      val file = chooser.getSelectedFile
      val node = SentinelTreeHelpers.makeTreeNodeFromProject( file )
      languageManager.addProject( node,
				 ( languageNode: DefaultMutableTreeNode ) =>
				   makeLanguage( languageNode ) )
      components.model.insertNodeIntoRoot( node )
    }
  }

  /**
   * Triggered when the user performs a menu action.
   * @param event The event correlating to the menu action
   */
  def actionPerformed( event: ActionEvent ) {
    val command = event.getActionCommand
    if ( actions.contains( command ) ) {
      actions( command )()
    }
  }

  /**
   * Exits the program
   */
  def exitAction() {
    System.exit( 0 )
  }
}
