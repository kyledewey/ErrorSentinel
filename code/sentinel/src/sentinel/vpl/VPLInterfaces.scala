/*
 * VPLInterfaces.scala
 *
 * Version:
 *     $Id: VPLInterfaces.scala,v 1.1 2011/06/20 22:37:52 kyledewey Exp $
 *
 * Revisions:
 *      $Log: VPLInterfaces.scala,v $
 *      Revision 1.1  2011/06/20 22:37:52  kyledewey
 *      Initial revision
 *
 *
 */

package sentinel.vpl

import sentinel.model._
import javax.swing._
import javax.swing.event._
import java.awt._
import java.awt.event._

/**
 * Base for the VPL.
 * The VPL simply needs to know when a particular node has been selected.
 * @author Kyle Dewey
 */
trait BaseVPL[ T <: AnyRef, U ] {
  /**
   * Says that the given node has been selected
   * @param node The node that has been selected
   */
  def nodeSelected( node: Node[ T, U ] ): Unit

  /**
   * Shows a description of the given describable thing
   * @param item The describable item
   */
  def showDescription( item: Describable ): Unit
}

/**
 * View of the GUI that the components tree gets.
 * The components tree should have access to only a restricted set of methods
 * that are in the full GUI, and this makes sure that it can access only
 * those that are applicable to it.
 * @author Kyle Dewey
 */
trait ComponentsTreeGUIView[ T <: AnyRef, U ] extends BaseVPL[ T, U ] {
  /**
   * Tells us where we are dragging to, relative to the components tree.
   * @param event The mouse event correlating to the drag
   */
  def treeDragging( event: MouseEvent ): Unit

  /**
   * Says that the node that we were dragging out has been released.
   * @param event The mouse event on the board that correlates to the release
   */
  def nodeReleased( event: MouseEvent ): Unit

  /**
   * Says that the given node is being dragged away
   * from the tree that held it.
   * @param node The node that is being moved
   */
  def nodeDraggingOut( node: Node[ T, U ] ): Unit
}

/**
 * View of the GUI that the visual board gets.
 * The visual board needs to tell the GUI when we are and are not in the board.
 * @author Kyle Dewey
 */
trait VisualBoardGUIView[ T <: AnyRef, U ] extends BaseVPL[ T, U ] {
  /**
   * Says that the mouse is in the board.
   */
  def mouseEnteredBoard(): Unit

  /**
   * Says that the mouse exited the board.
   */
  def mouseExitedBoard(): Unit

  /**
   * Gets all the available languages that are known to the GUI.
   * @return All languages known to the GUI
   */
  def languageManager(): LanguageManager
}

/**
 * A complete VPL interface.
 * @author Kyle Dewey
 */
trait CompleteVPL[ T <: AnyRef, U ] 
extends ComponentsTreeGUIView[ T, U ] with VisualBoardGUIView[ T, U ] {}
