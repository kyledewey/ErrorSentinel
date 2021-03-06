/*
 * ContextSensitivePopupTable.scala
 */

package sentinel.utils.interactive

import java.awt._
import java.awt.event._
import javax.swing._
import javax.swing.event._
import javax.swing.table._

/**
 * A table that creates context-sensitive popup menus.
 * Note that for such popups to work properly, only single selection
 * can be allowed.
 * @author Kyle Dewey
 */
trait ContextSensitivePopupTable extends SingleSelectionTable {
  // begin constructor
  addMouseListener( makePopupMouseListener )
  // end constructor

  /**
   * Makes a popup menu for the given row and column.
   * If there isn't an appropriate popup menu, this returns None.
   * By default, this returns None.  Note that this is guarenteed to be
   * called for a valid cell.
   * @param row The row of the popup
   * @param column The column of the popup
   * @return A context-sensitive popup menu
   */
  def makePopupMenu( row: Int, column: Int ): Option[ JPopupMenu ] =
    None

  /**
   * Like <code>makePopupMenu</code>, but it works with a point.
   * @param point The point that was clicked.
   * @return A context-sensitive JPopupMenu, or None if there isn't
   * an appropriate menu or if the point doesn't correspond to a valid cell.
   */
  def makePopupMenu( point: Point ): Option[ JPopupMenu ] = 
    callIfInCell( point, makePopupMenu( _, _ ) ).getOrElse( None )
  
  /**
   * Makes a popup menu that is relevant to the given mouse event.
   * @param event The mouse event that is relevant to the click
   * @return A context-sensitive JPopupMenu for the click.  If there
   * isn't an appropriate menu, then this returns None.
   */
  def makePopupMenu( event: MouseEvent ): Option[ JPopupMenu ] =
    if ( SwingUtilities.isRightMouseButton( event ) ) {
      makePopupMenu( event.getPoint )
    } else {
      None
    }
  
  /**
   * Makes the given popup appear under the given cell.
   * The popup is centered underneath the cell.
   * @param popup The popup to show
   * @param row The row of the cell
   * @param column The column of the cell
   */
  def showPopup( popup: JPopupMenu, row: Int, column: Int ) {
    val rect = getCellRect( row, column, true )
    popup.show( this, 
	        rect.getX.asInstanceOf[ Int ],
	        ( rect.getY + rect.getHeight ).asInstanceOf[ Int ] )
  }

  /**
   * Makes the mouse listener for popups.
   * @return The mouse listener for popups
   */
  protected def makePopupMouseListener() = 
    new MouseListener() {
      def mouseClicked( event: MouseEvent ) {
	setSelectedCell( event.getPoint )
	val popup = makePopupMenu( event )
	if ( popup.isDefined ) {
	  callIfSelected( ( row: Int, column: Int ) =>
	    showPopup( popup.get, row, column ) )
	}
      }
      def mouseEntered( e: MouseEvent ) {}
      def mouseExited( e: MouseEvent ) {}
      def mousePressed( e: MouseEvent ) {}
      def mouseReleased( e: MouseEvent ) {}
    }
} // ContextSensitivePopupMenu

