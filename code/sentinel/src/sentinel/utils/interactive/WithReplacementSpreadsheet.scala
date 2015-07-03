/*
 * WithReplacementSpreadsheet.scala
 *
 * Version:
 *     $Id: WithReplacementSpreadsheet.scala,v 1.7 2011/06/04 07:38:16 kyledewey Exp $
 *
 * Revisions:
 *      $Log: WithReplacementSpreadsheet.scala,v $
 *      Revision 1.7  2011/06/04 07:38:16  kyledewey
 *      Major refactor for use with AssociationGraph.
 *      Now updates the graph properly under the correct conditions.
 *      tryValueAt is now called semi-recursively for values that
 *      use changed values.
 *
 *      Revision 1.6  2011/06/04 05:19:12  kyledewey
 *      Now implements AssociationGraph.
 *
 *      Revision 1.5  2011/05/29 22:15:25  kyledewey
 *      Fixed bugs that would cause more than one evaluation
 *      to be performed when performing an autocorrection.
 *      Added support for removing rows.
 *
 *      Revision 1.4  2011/05/29 15:21:20  kyledewey
 *      Added the project being created to the spreadsheetFactory.
 *
 *      Revision 1.3  2011/05/28 02:41:38  kyledewey
 *      tryValueAtAllCells() now uses foreachRowColumn().
 *
 *      Revision 1.2  2011/05/27 18:52:09  kyledewey
 *      Added the forceRender() method.
 *      Changed the name of tableChanging to
 *      the more accurrate shouldRespond.
 *
 *      Revision 1.1  2011/05/27 01:37:30  kyledewey
 *      Initial revision
 *
 *
 */

package sentinel.utils.interactive

import javax.swing.event._
import sentinel.model._
import sentinel.project._

/**
 * Cell contents for a <code>WithReplacementSpreadsheet</code>
 * @param instanceResult The last instance result for this cell
 * @author Kyle Dewey
 */
class WithReplacementCellContents( var instanceResult: Option[ Option[ InstanceResult ] ] ) extends NonDataCellContents {
  /**
   * Creates a new instance with None set as the
   * instance result.
   */
  def this() =
    this( None )
}

/**
 * Holds methods to simplify the creation of
 * <code>WithReplacementSpreadsheet</code>s.
 * @author Kyle Dewey
 */
object WithReplacementSpreadsheet {
  /**
   * Creates a new <code>WithReplacementSpreadsheet</code>
   * @param name The name of the spreadsheet
   * @param project The project that we are associated with
   * @param register Whether or not to register the spreadsheet
   * @param instantiator Something that can make new instances of
   * <code>WithReplacementCellContents</code>
   * @return A new <code>WithReplacementSpreadsheet</code> instantiated
   * with the give params.
   */
  def apply[ T <: WithReplacementCellContents ]( name: String, project: Project[ _ ], register: Boolean, instantiator: ( Int, Int ) => T ): WithReplacementSpreadsheet[ T ] =
    new WithReplacementSpreadsheet[ T ]( name, project, register, instantiator )

  /**
   * Creates a new <code>WithReplacementSpreadsheet</code> that simply
   * uses <code>WithReplacementCellContents</code> as-is for the cell contents.
   * @param name The name of the spreadsheet
   * @param project The project that we are associated with
   * @param register Whether or not to register the spreadsheet
   * @return A new <code>WithReplacementSpreadsheet</code> using the given
   * params that makes default <code>WithReplacementCellContents</code>
   * objects.
   */
  def apply( name: String, project: Project[ _ ], register: Boolean ): WithReplacementSpreadsheet[ WithReplacementCellContents ] =
    apply( name, project, register, ( row: Int, column: Int ) => new WithReplacementCellContents() )

  /**
   * Creates a new <code>WithReplacementSpreadsheet</code> holding the
   * contents of another spreadsheet.
   * @param sheet The spreadsheet to base this one on
   * @param project The project that we are associated with
   * @param register Whether or not to register this spreadsheet
   * @return A new spreadsheet based on the given spreadsheet
   */
  def apply( sheet: Spreadsheet, project: Project[ _ ], register: Boolean ): WithReplacementSpreadsheet[ WithReplacementCellContents ] = 
    Spreadsheet.copyContents( sheet, apply( sheet.name, project, register ) )

  /**
   * For each row and column in a table model event, it will call
   * the given function.  Note that the function will not be called for
   * metadata events.
   * @param event The table model event
   * @param function The function to call.  Takes the current row and
   * column as params
   */
  def foreachRowColumn( event: TableModelEvent, function: ( Int, Int ) => Unit ) {
    import javax.swing.table._
    if ( event.getFirstRow != TableModelEvent.HEADER_ROW ) {
      val rows = event.getFirstRow.to( event.getLastRow )
      val columns =
	if ( event.getColumn == TableModelEvent.ALL_COLUMNS ) {
	  0.until( event.getSource.asInstanceOf[ TableModel ].getColumnCount ).toSeq
	} else {
	  Seq( event.getColumn )
	}
      rows.foreach( row =>
	columns.foreach( column =>
	  function( row, column ) ) )
    }
  }
			  
}

/**
 * Spreasheet that holds the most recent replacement results for a cell.
 * @param name The name of the spreadsheet
 * @param project The project that the given spreadsheet is associated with.
 * @param register Whether or not to register this spreadsheet
 * @param instantiator Something that can instantiate new
 * <code>WithReplacementCellContents</code> on demand.
 * @author Kyle Dewey
 */
class WithReplacementSpreadsheet[ T <: WithReplacementCellContents ]( name: String, val project: Project[ _ ], register: Boolean, instantiator: ( Int, Int ) => T )
extends DefaultReplacerSpreadsheet[ T ]( name, register, instantiator ) 
with AssociationGraph {
  import java.util.Vector

  // begin instance variables
  // set to true if we should respond to table changed events
  // note that both rendering updates and an actual change to table
  // data trigger the same event, but we should only respond to
  // the actual data changing
  var shouldRespond = true

  // set to true if we should do a recursive tryValueAt, taking into
  // account downstream changes
  var shouldRecurse = true

  // end instance variables

  // begin constructor
  addTableModelListener( makeWithReplacementSpreadsheetListener )
  addRowIfEmpty()
  // end constructor

  /**
   * Puts the given instance result in the given cell.
   * Note that this will overwrite whatever instance result is already
   * in this cell.
   * @param row The row
   * @param column The column
   * @param instanceResult The instance result to put in this cell.
   * @throws IndexOutOfBoundsException If the given position doesn't exist
   */
  def putInstanceResult( row: Int, 
			 column: Int, 
			 instanceResult: Option[ InstanceResult ] ) {
    getInstantiatedDataAt( row, column ).instanceResult = Some( instanceResult )
  }
  
  /**
   * Gets an instance result from the given cell.
   * @param row The row of the cell
   * @param column The column of the cell
   * @return The instance result at this cell, or None if there isn't
   * one that has been set yet.
   * @throws IndexOutOfBoundsException If the given position doesn't exist
   */
  def getInstanceResult( row: Int, column: Int ) =
    getInstantiatedDataAt( row, column ).instanceResult

  /**
   * Does everything that the superclass' <code>tryValueAt</code> does, only
   * it saves the instance result returned in addition to returning it.
   * @param value The value to try at this position
   * @param row The row to try the value at
   * @param column The column to try the value at
   * @return The result of running the error correction language
   * against it.
   */
  override def tryValueAt( value: String, row: Int, column: Int ) = {
    shouldRespond = false
    val superResult = super.tryValueAt( value, row, column )
    putInstanceResult( row, column, superResult )
    if ( shouldRecurse ) {
      shouldRecurse = false
      usedAsParameterCells( row, column ).foreach( pair =>
	if ( pair._1 != row || pair._2 != column ) {
	  tryValueAt( pair._1, pair._2 ) 
	} )
      shouldRecurse = true
    }
    shouldRespond = true
    superResult
  }

  /**
   * Calls <code>tryValueAt</code> for the given row and column using
   * the data that is already there.  Note that if there isn't anything
   * there (null), then it is converted to a null string.
   * @param row The row to call it at
   * @param column The column to call it at
   * @return The result of running the error correction language against
   * the value at the spreadsheet at this location
   */
  def tryValueAt( row: Int, column: Int ): Option[ InstanceResult ] = {
    val valueHere = getValueAt( row, column )
    val value =
      if ( valueHere == null ) ""
      else valueHere.toString

    tryValueAt( value, row, column )
  }

  /**
   * Forces the given cell to be rendered.
   * @param row The row of the cell
   * @param column The column of the cell
   */
  def forceRender( row: Int, column: Int ) {
    shouldRespond = false
    fireTableCellUpdated( row, column )
    shouldRespond = true
  }

  /**
   * Updates both the project associations and the graph assocations.
   * @param event The event correlating to a table model change
   */
  def updateAssociations( event: TableModelEvent ) {
    if ( shouldRespond ) {
      if ( event.getType == TableModelEvent.INSERT ) {
	project.associations.rowAdded( this, event.getFirstRow )
	resetGraph()
      } else if ( event.getType == TableModelEvent.DELETE ) {
	project.associations.rowRemoved( this, event.getFirstRow )
	resetGraph()
      }
    }
  }

  /**
   * Makes a listener that upon the change of a table,
   * it will update the instance results.
   * @return a table model listener that will update instance results based
   * on a table change
   */
  def makeWithReplacementSpreadsheetListener() =
    new TableModelListener() {
      def tableChanged( event: TableModelEvent ) {
	updateAssociations( event )
	tryValueAtEvent( event )
      }
    }

  /**
   * Calls <code>tryValueAt</code> for the given range dictated by
   * a table model event.
   * @param event The table model event
   */
  def tryValueAtEvent( event: TableModelEvent ) {
    if ( shouldRespond ) {
      shouldRespond = false
      WithReplacementSpreadsheet.foreachRowColumn( event,
						   tryValueAt( _, _ ) )
      shouldRespond = true
    }
  }

  /**
   * Calls <code>tryValueAt</code> for the given row of the sheet.
   * @param row The row to call it on
   */
  def tryValueAtRow( row: Int ) {
    shouldRespond = false
    0.until( getColumnCount ).foreach( tryValueAt( row, _ ) )
    shouldRespond = true
  }

  /**
   * Calls <code>tryValueAt</code> for all rows between the given
   * range of rows, inclusive.
   * @param start The starting row
   * @param end The ending row
   */
  def tryValueAtRows( start: Int, end: Int ) {
    start.to( end ).foreach( tryValueAtRow( _ ) )
  }

  /**
   * Calls <code>tryValueAt</code> for all cells.
   */
  def tryValueAtAllCells() {
    shouldRespond = false
    foreachRowColumn( tryValueAt( _, _ ) )
    shouldRespond = true
  }

  /**
   * Removes a row from the spreadsheet.
   * @param row The row to remove
   */
  override def removeRow( row: Int ) {
    shouldRespond = false
    super.removeRow( row )
    project.associations.rowRemoved( this, row )
    shouldRespond = true
    addRowIfEmpty()
  }

  /**
   * Inserts a row in the spreadsheet
   * This will update any good data matcher and error correction
   * pairs.  I.e. if something's cell range extends to this portion,
   * it will fill it in.  Note that the Vector addRow is the addRow
   * entrance point for DefaultTableModel
   * @param row The row to add at
   * @param rowData The data to add
   */
  override def insertRow( row: Int, rowData: Vector[ _ ] ) {
    shouldRespond = false
    super.insertRow( row, rowData )
    shouldRespond = true
    tryValueAtRows( row, getRowCount - 1 )
  }

  /**
   * Adds a column to the end of this spreadsheet.
   * @param columnName The name of the column
   * @param data The data for the column
   */
  override def addColumn( columnName: Object, data: Vector[ _ ] ) {
    shouldRespond = false
    super.addColumn( columnName, data )
    shouldRespond = true
  }
}

