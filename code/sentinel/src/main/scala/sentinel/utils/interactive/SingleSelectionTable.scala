/*
 * SingleSelectionTable.scala
 */

package sentinel.utils.interactive

import java.awt._
import java.awt.event._
import javax.swing._
import javax.swing.event._
import javax.swing.table._

/**
 * The selection model used for the single selection table.
 * @author Kyle Dewey
 */
class SingleSelectionModel extends DefaultListSelectionModel {
  setSelectionMode( ListSelectionModel.SINGLE_SELECTION )
}

/**
 * Makes a table allow for only single selection.
 * @author Kyle Dewey
 */
trait SingleSelectionTable extends JTable {
  // begin instance variables
  val rsModel = new SingleSelectionModel() // row selection model
  val csModel = new SingleSelectionModel() // column selection model
  // end instance variables

  // begin constructor
  setSelectionModel( rsModel )
  getColumnModel.setSelectionModel( csModel )
  setSelectionMode( ListSelectionModel.SINGLE_SELECTION )
  setRowSelectionAllowed( false )
  // end constructor

  /**
   * Sets the selected cell to the given cell.
   * @param row The row of the cell
   * @param column The column of the cell
   */
  def setSelectedCell( row: Int, column: Int ) {
    setRowSelectionInterval( row, row )
    setColumnSelectionInterval( column, column )
  }

  /**
   * Sets the selected cell to the one that is at the given point.
   * If the point does not point to a cell, this does nothing.
   * @param point the point
   */
  def setSelectedCell( point: Point ) {
    callIfInCell( point, setSelectedCell( _, _ ) )
  }

  /**
   * Calls the given function if the given point correlates to a row
   * and column.  If the point does not point to a valid cell, then
   * the function isn't called.
   * @param point The point
   * @param function The function to call
   * @return Some( functionResult ) if the given point is a point, else
   * None
   */
  def callIfInCell[ T ]( point: Point, function: ( Int, Int ) => T ) = {
    val row = rowAtPoint( point )
    val column = columnAtPoint( point )
    if ( row != -1 && column != -1 ) {
      Some( function( row, column ) )
    } else {
      None
    }
  }
  
  /**
   * If calls the given function with the currently selected
   * row and column, but only if there is a currently selected
   * row and column.  Otherwise it doesn't do anything of use.
   * @param function Function to call with the currently selected
   * row and column
   * @return Some( T ) if we have a currently selected row and column,
   * otherwise None
   */
  def callIfSelected[ T ]( function: ( Int, Int ) => T ): Option[ T ] = {
    val row = getSelectedRow
    val column = getSelectedColumn

    if ( row != -1 && column != -1 ) {
      Some( function( row, column ) )
    } else {
      None
    }
  }
}

