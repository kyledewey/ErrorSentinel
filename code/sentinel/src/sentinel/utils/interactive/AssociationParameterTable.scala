/*
 * AssociationParameterTable.scala
 * 
 * Version:
 *     $Id: AssociationParameterTable.scala,v 1.2 2011/05/31 18:44:46 kyledewey Exp $
 *
 * Revisions:
 *      $Log: AssociationParameterTable.scala,v $
 *      Revision 1.2  2011/05/31 18:44:46  kyledewey
 *      Now compiles correctly.
 *
 *      Revision 1.1  2011/05/31 17:20:58  kyledewey
 *      Initial revision
 *
 *
 */

package sentinel.utils.interactive

import java.awt.event._
import javax.swing._
import javax.swing.event._
import javax.swing.table._

import sentinel.model._
import ParamType._

/**
 * Holds constants for AssociationParameterTableModel.
 * @author Kyle Dewey
 */
object AssociationParameterTableModel {
  val PARAMETER_NAME = "Name"
  val IS_ARRAY = "Is Array?"
  val IS_REQUIRED = "Is Required?"
  val VALUE = "Value"
  // given an ID, it returns a name
  val COLUMN_NAMES = 
    Array[ Object ]( PARAMETER_NAME,
		     IS_ARRAY,
		     IS_REQUIRED,
		     VALUE )

  // given a name, it returns an ID
  val COLUMN_IDS = 
    Map() ++ 0.until( COLUMN_NAMES.size ).map( id =>
      Pair( COLUMN_NAMES( id ), id ) )

  /**
   * Determines if the given column is the value column
   * @param column The column to test
   * @return true if it's the value column, else false
   */
  def isValueColumn( column: Int ) =
    column == COLUMN_IDS( VALUE )

  /**
   * Makes the data for a row of the table.
   * @param paramInfo The param info object describing the parameter.
   * @return A row of data for the table
   */
  def makeRowData( paramInfo: ParamInfo ): Array[ Object ] = 
    COLUMN_NAMES.map( columnName =>
      columnName match {
	case PARAMETER_NAME => paramInfo.name
	case IS_ARRAY => Predef.boolean2Boolean( paramInfo.isArray )
	case IS_REQUIRED => Predef.boolean2Boolean( paramInfo.isRequired )
	case VALUE => null
      } ).toArray

  /**
   * Given an instance factory, it will make data for the table based on
   * that instance factory.
   * @param factory The instance factory.
   * @return row data for the instance factory.
   */
  def makeRowData( factory: InstanceFactory[ _ ] ): Array[ Array[ Object ] ] =
    factory.mapParamInfos( makeRowData( _ ) ).toArray

  /**
   * Given a string holding what the user typed in for a value, it
   * returns what the object is.
   * @param string The string that the user typed in
   * @return The object that should be used, or None if such a determination
   * could not be made.
   */
  def userValueToObject( string: String ): Option[ Object ] =
    if ( string.startsWith( "\"" ) && string.endsWith( "\"" ) ) {
      Some( string )
    } else {
      CellRange.parseCellRange( string )
    }
}

import AssociationParameterTableModel._

/**
 * The data model for the association parameter table.
 * @param factory The underlying factory this model is for
 * @author Kyle Dewey
 */
class AssociationParameterTableModel( val factory: InstanceFactory[ _ ] )
extends DefaultTableModel( makeRowData( factory ), COLUMN_NAMES ) {
  private var _errorMessage: Option[ String ] = None

  /**
   * Gets the error message associated with the last setValueAt.
   * Note that this will clear the error message
   * @return The last error message, or None if there wasn't an error
   */
  def errorMessage() = {
    val retval = _errorMessage
    _errorMessage = None
    retval
  }

  /**
   * Sets the error message to the given value.
   * Overwrites the last value of the message.
   * @param value The value to set it to
   */
  protected def setError( message: String ) {
    _errorMessage = Some( message )
  }

  /**
   * The only column that we allow editing on is the last one holding
   * the value.
   * @param row The row that we are trying to edit
   * @param column The column that we are trying to edit
   * @return true if it is the value column, else false
   */
  override def isCellEditable( row: Int, column: Int ) =
    isValueColumn( column )

  /**
   * Sets the value at the given row and column.
   * If we are setting the value, this will validate the value.  If it's
   * not valid, it will refuse to set it.
   * @param value The value to try to set
   * @param row The row to set
   * @param column The column to set
   */
  override def setValueAt( value: Object, row: Int, column: Int ) {
    if ( !isValueColumn( column ) ||
         value == null ) {
      super.setValueAt( value, row, column )
    } else {
      val converted = userValueToObject( value.toString )
      if ( converted.isEmpty ) {
	setError( "Could not convert value to either a string or a cell range." )
	super.setValueAt( null, row, column )
      } else {
	super.setValueAt( converted.get, row, column )
      }
    }
  }
}
  
/**
 * Table that allows for parameters to be associated with instances.
 * @param model The model to use
 * @author Kyle Dewey
 */
class AssociationParameterTable( val model: AssociationParameterTableModel )
extends JTable( model ) with ErrorShower with ContextSensitivePopupTable with RowMover {
  // begin constructor
  model.addTableModelListener( makeInsertErrorTableModelListener ) 
  // end constructor

  /**
   * Makes the table model listener that will look for erroneous inserts.
   * @return A table model listener that watches for erroneous inserts
   */
  protected def makeInsertErrorTableModelListener() =
    new TableModelListener() {
      def tableChanged( event: TableModelEvent ) {
	val error = model.errorMessage
	if ( error.isDefined ) {
	  showError( error.get )
	}
      }
    }

  /**
   * Moves the given range of rows to the given position.
   * @param start The start of the range
   * @param end The end of the range
   * @param to Where to put the rows
   */
  def moveRows( start: Int, end: Int, to: Int ) {
    if ( start != to ) {
      model.moveRow( start, end, to )
    }
  }
}
