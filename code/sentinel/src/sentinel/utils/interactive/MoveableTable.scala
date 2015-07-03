/*
 * MoveableTable.scala
 *
 * Version:
 *     $Id: MoveableTable.scala,v 1.2 2011/05/31 18:44:46 kyledewey Exp $
 *
 * Revisions:
 *      $Log: MoveableTable.scala,v $
 *      Revision 1.2  2011/05/31 18:44:46  kyledewey
 *      Now applies only to JTables.
 *      Listener is automatically registered.
 *
 *      Revision 1.1  2011/05/31 00:08:17  kyledewey
 *      Initial revision
 *
 *
 */

package sentinel.utils.interactive

import java.awt._
import java.awt.event._
import javax.swing._

/**
 * Something that watches for rows being moved.
 * @author Kyle Dewey
 */
trait RowMover extends JTable {
  // begin constructor
  addMouseListener( makeRowMoverMouseListener )
  // end constructor

  /**
   * Makes the row mover mouse listener
   * @return The row mover mouse listener
   */
  protected def makeRowMoverMouseListener() =
    new MouseListener() {
      // the row that is currently being dragged
      private var rowDragging: Option[ Int ] = None
      
      def mouseClicked( e: MouseEvent ) {}
      def mouseEntered( e: MouseEvent ) {}
      def mouseExited( e: MouseEvent ) {}
      def mousePressed( e: MouseEvent ) {
	val row = rowAtPoint( e.getPoint )
	rowDragging =
	  if ( row != -1 ) {
	    Some( row )
	  } else {
	    None
	  }
      }
      def mouseReleased( e: MouseEvent ) {
	lazy val oldRow = rowDragging.get
	lazy val newRow = rowAtPoint( e.getPoint )
	
	if ( rowDragging.isDefined &&
            newRow != -1 ) {
	  moveRow( oldRow, newRow )
	}
      }
    }

  /**
   * Moves the given row to the given position.
   * @param row The row to move
   * @param to Where to move the row
   */
  def moveRow( row: Int, to: Int ) {
    moveRows( row, row, to )
  }

  /**
   * Moves the given range of rows to the given location.
   * Note that the range is inclusive
   * @param start The start of the range
   * @param end The end of the range
   * @param to Where to move the rows to
   */
  def moveRows( start: Int, end: Int, to: Int ): Unit
}
