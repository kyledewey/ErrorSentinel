/*
 * Spreadsheet.scala
 * 
 * Version:
 *     $Id: Spreadsheet.scala,v 1.11 2011/06/17 19:51:52 kyledewey Exp $
 *
 * Revisions:
 *      $Log: Spreadsheet.scala,v $
 *      Revision 1.11  2011/06/17 19:51:52  kyledewey
 *      Now uses strings instread of rich strings for keys.
 *      Far more efficient without the need for constant wrappings.
 *
 *      Revision 1.10  2011/06/04 05:11:02  kyledewey
 *      Added the mapTable() method.
 *
 *      Revision 1.9  2011/05/31 17:17:48  kyledewey
 *      Added the validateSheetName() method.
 *      This method is used for sheet name validation
 *      regardless of whether or not the sheet is registered.
 *
 *      Revision 1.8  2011/05/29 22:11:57  kyledewey
 *      Added the insertRow( Int ), addRowIfEmpty(),
 *      and removeRow() methods.  Fixed bug in insertRow()
 *      where the parallel data was always put at the end, regardless
 *      of insert position.
 *
 *      Revision 1.7  2011/05/29 15:21:51  kyledewey
 *      Added the inRange() method to Spreadsheet.
 *
 *      Revision 1.6  2011/05/28 02:37:45  kyledewey
 *      Added the foreachElement() and foreachRowColumn()
 *      methods.
 *
 *      Revision 1.5  2011/05/27 18:48:02  kyledewey
 *      Added the updateCurrent() methods.
 *
 *      Revision 1.4  2011/05/27 01:32:13  kyledewey
 *      Added the copyContents() method.
 *
 *      Revision 1.3  2011/05/25 20:04:57  kyledewey
 *      Added the LazyParallelSpreadsheet class.
 *      Added checks to prevent NPEs on parallel if it isn't
 *      yet initialized.
 *
 *      Revision 1.2  2010/07/17 02:04:01  kyledewey
 *      Refactored spreadsheet hierarchy so that Spreadsheet and
 *      ReplacerSpreadsheet are both traits instead of classes.
 *
 *      Revision 1.1  2010/07/11 05:44:00  kyledewey
 *      Initial revision
 *
 *
 */

package sentinel.model

import scala.runtime._
import scala.collection.mutable.ArrayBuffer
import javax.swing.table.DefaultTableModel
import java.util.Vector

/**
 * Exception thrown when the name of a spreadsheet is invalid.
 * @param message A helpful message to show the user
 * @author Kyle Dewey
 */
case class SpreadsheetNameException( message: String ) 
     extends Exception( message ) {}

/**
 * Keeps track of what the current sheet, row, and column is.
 * This is needed for * in variables.
 * @author Kyle Dewey
 */
object Spreadsheet {
  // begin constants
  val DELIMETER = ":"
  val ANY_SHEET = "*"
  val ANY_ROW = ANY_SHEET
  val ANY_COLUMN = ANY_ROW
  // end constants

  // begin public variables
  var currentSpreadsheet = ""
  var currentRow = 0
  var currentColumn = 0
  // end public variables

  // begin private variables
  var spreadsheets: Map[ String, Spreadsheet ] = Map()
  // end private variables

  /**
   * Updates the current sheet, row, and column to the given values.
   * Assumes that these values are valid.
   * @param sheet The new current spreadsheet
   * @param row The new current row
   * @param column The new current column
   */
  def updateCurrent( sheet: String, row: Int, column: Int ) {
    currentSpreadsheet = sheet
    currentRow = row
    currentColumn = column 
  }

  /**
   * Creates a spreadseet with the given name.  The spreadsheet will be
   * registered upon creation.
   * @param name The name of the spreadsheet
   * @throws SpreadsheetNameException If the name of the spreadsheet is invalid
   */
  def apply( name: String ) =
    new DefaultSpreadsheet( name )

  /**
   * Creates a spreadsheet with the given name, and whether or not the
   * spreadsheet should be registered.
   * @param name The name of the spreadsheet
   * @param register Whether or not the spreadsheet should be registered
   * @throws SpreadsheetNameException If the name of the spreadsheet is invalid
   */
  def apply( name: String, register: Boolean ) =
    new DefaultSpreadsheet( name, register )

  /**
   * Makes the second spreadsheet hold the data and column identifiers
   * of the first spreadsheet.  Note that this is destructive, and
   * the copy made is shallow.
   * @param source The source spreadsheet
   * @param dest The destination spreadsheet
   * @return The destination spreadsheet, to allow for chaining calls
   */
  def copyContents[ T <: Spreadsheet ]( source: Spreadsheet, dest: T ) = {
    dest.setDataVector( source.getDataVector,
		        source.getColumnIdentifiers )
    dest
  }

  /**
   * Registers the given spreadsheet.
   * If the given sheet already exists, it will be
   * overridden.
   * @param sheet The spreadsheet itself
   */
  def registerSpreadsheet( sheet: Spreadsheet ) {
    spreadsheets += Pair( sheet.name, sheet )
  }

  /**
   * Unregisters the given spreadsheet.
   * If the given spreadsheet doesn't exist, then nothing
   * will happen
   * @param name The name of the spreadsheet to remove
   */
  def unregisterSpreadsheet( name: String ) {
    if ( spreadsheets.contains( name ) ) {
      val sheet = spreadsheets( name )
      spreadsheets -= name
    }
  }

  /**
   * Determines if the given spreadsheet exists
   * @param name The name of the spreasheet
   * @return true if the given spreadsheet has been registered, else false
   */
  def spreadsheetExists( name: String ) = 
    spreadsheets.contains( name )

  /**
   * Gets the spreadsheet with the given name
   * @param name The name of the spreasheet to get
   * @return The spreadsheet, or None if there was no such spreadsheet
   */
  def getSpreadsheet( name: String ) =
    spreadsheets.get( name )

  /**
   * Gets the spreadsheet with the given name.
   * If None is passed, then the current spreadsheet is returned.
   * @param name The name of the spreadsheet to get, or None
   * if the current spreadsheet
   * @return The spreadsheet, or the current spreadsheet if None.  None
   * if a specific spreadsheet was passed but it does not correspond
   * to any spreadsheet we know of.
   */
  def getSpreadsheet( name: Option[ String ] ): Option[ Spreadsheet ] = {
    if ( name.isEmpty ) getSpreadsheet( currentSpreadsheet ) 
    else getSpreadsheet( name.get )
  }

  /**
   * Gets the names of all spreasheets, in abc order.
   * @return The names of all registered sheets, in abc order
   */
  def getSpreadsheets() =
    spreadsheets.keys.toList.sortWith( _.compareTo( _ ) < 0 )

  /**
   * Validates the given spreadsheet name.
   * Spreadsheet names cannot contain quotes, the delimiter, and cannot
   * be ANY_SHEET.
   * @param name The name of the sheet to validate
   * @throws SpreadsheetNameException If the sheet name is invalid
   */
  def validateSheetName( name: String ) {
    var errorString: Option[ String ] = None

    if ( name == ANY_SHEET ) {
      errorString = Some( "Spreadsheet name cannot be the " +
			  " same as the name for any " +
			  " spreadsheet: " +
			  Spreadsheet.ANY_SHEET )
    } else if ( name.indexOf( "\"" ) != -1 ) {
      errorString = Some( "Spreadsheet name cannot contain double quotes." )
    } else if ( name.indexOf( DELIMETER ) != -1 ) {
      errorString = Some( "Spreadsheet name cannot contain delimiters: \"" +
			  DELIMETER + "\"" )
    }

    if ( errorString.isDefined ) {
      throw new SpreadsheetNameException( errorString.get )
    }
  }
}

/**
 * Represents a spreadsheet in Scala.
 * This is merely the normal table model made more Scala friendly.
 * @author Kyle Dewey
 */
trait Spreadsheet extends DefaultTableModel {
  /**
   * Gets the name of the spreadsheet.
   * @return The name of the spreadsheet
   */
  def name(): String 

  /**
   * Determines if the given row and column is contained within
   * this spreadsheet.
   * @param row The row
   * @param column The column
   * @return true if it's in the sheet, else false
   */
  def inRange( row: Int, column: Int ) =
    ( row >= 0 && 
      row < getRowCount &&
      column >= 0 && 
      column < getColumnCount )
    
  /**
   * Updates the current spreadsheet, current row, and current column.
   * Uses the name as the current sheet.
   * @param row The current row
   * @param column The current column
   */
  def updateCurrent( row: Int, column: Int ) {
    Spreadsheet.updateCurrent( name, row, column )
  }

  /**
   * Adds a column to the end of the model
   * The column will have only the name of its position
   */
  def addColumn(): Unit =
    addColumn( None )
  
  /**
   * Adds a column to the end of the model
   * @param name The name of the column to add; none if no name
   */
  def addColumn( name: Option[ Object ] ): Unit =
    addColumn( name, None )

   /**
   * Adds a column to the end of the model
   * @param name The name of the column to add; none if no name
   * @param columnData The data to put into; none if no data
   */
  def addColumn( name: Option[ Object ], columnData: Option[ Seq[ Object ] ] ) {
    val data = 
      if ( columnData.isDefined ) columnData.get.toArray
      else null
    
    addColumn( name.getOrElse( ( getColumnCount + 1 ).toString ), 
	       data )
  }

  /**
   * Adds a new blank row to the end of the model.
   */
  def addRow() {
    addRow( None )
  }

  /**
   * Adds a row to the end of this spreadsheet if the spreadsheet
   * doesn't contain any rows.
   */
  def addRowIfEmpty() {
    if ( getRowCount == 0 ) {
      addRow()
    }
  }

  /**
   * Adds a row to the end of the model
   * @param rowData The optional data to add
   */
  def addRow( rowData: Option[ Seq[ Object ] ] ) {
    val row =
      if ( rowData.isDefined ) rowData.get.toArray
      else null
    addRow( row )
  }

  /**
   * Inserts a blank row at an arbitrary position in the model.
   * @param row The position where to insert the row
   * @throws ArrayIndexOutOfBoundsException If the row was invalid
   */
  def insertRow( row: Int ) {
    val typedNull: Vector[ _ ]= null
    insertRow( row, typedNull )
  }

  /**
   * Gets all the values at the given row
   * @param row The row to get all the values of
   * @return All the values at the given row, or None if the row
   * doesn't exist
   */
  def row( row: Int ): Option[ Seq[ Object ] ] = {
    if ( row >= 0 &&
        row < getRowCount ) {
      Some( ( 0 to getColumnCount - 1 ).map( getValueAt( row, _ ) ) )
    } else None
  }

  /**
   * Gets all the values at the given column
   * @param column The column to get all the values of
   * @return All the values at the given column, or None if the
   * column doesn't exist
   */
  def column( column: Int ): Option[ Seq[ Object ] ] = {
    if ( column >= 0 &&
	 column < getColumnCount ) {
      Some( ( 0 to getRowCount - 1 ).map( getValueAt( _, column ) ) )
    } else None
  }

  /**
   * Goes over each element in the spreadsheet, calling the given
   * function on the row and column of the element.
   * @param function The function to call for each row and column 
   */
  def foreachRowColumn( function: ( Int, Int ) => Unit ) {
    0.until( getRowCount ).foreach( row => 
      0.until( getColumnCount ).foreach( column => 
	function( row, column ) ) )
  }

  /**
   * Makes a parallel table with the given initialization function.
   * @param initializer Used to initialize each row and column
   * @return A parallel table
   */
  def mapTable[T:ClassManifest]( initializer: ( Int, Int ) => T ) =
    SentinelHelpers.makeTable( getRowCount,
			       getColumnCount,
			       initializer )

  /**
   * Goes over each element in the spreadsheet, calling the
   * given function on the element.
   * @param function The function to call for each element of
   * the spreadsheet
   */
  def foreachElement( function: Object => Unit ) {
    foreachRowColumn( ( row: Int, column: Int ) =>
      function( getValueAt( row, column ) ) )
  }

  /**
   * Gets the identifiers for all columns.
   * @return The identifiers for all columns
   */
  def getColumnIdentifiers(): Vector[ _ ]
}

/**
 * Represents a spreadsheet.
 * @param name The name of the spreasheet
 * @param register If the spreadsheet should be registered
 * @throws SpreasheetNameException If the name of the spreadsheet is
 * invalid
 * @author Kyle Dewey
 */
class DefaultSpreadsheet( val name: String, 
			  val register: Boolean ) extends Spreadsheet {
  // begin constructor
  Spreadsheet.validateSheetName( name )
  // end constructor
  
  /**
   * Creates a new spreadsheet which will be registered
   * @param name The name of the spreadsheet to register
   * @throws SpreadsheetNameException If the name of the spreadsheet is invalid
   */
  def this( name: String ) =
    this( name, true )
  
  /**
   * Gets all column identifiers.
   * @return All column identifiers
   */
  override def getColumnIdentifiers() =
    columnIdentifiers
}

/**
 * Represents a spreadsheet that keeps track of a parallel matrix of other
 * items.
 * @param name The name of the spreadsheet
 * @param register whether or not to register this spreadsheet
 * @param parallel The parallel matrix of other items
 * @throws SpreadsheetNameException If the name of the spreadsheet is invalid
 * @author Kyle Dewey
 */
class ParallelSpreadsheet[ T ]( name: String,
			        register: Boolean )
extends DefaultSpreadsheet( name, register ) {
  // begin instance variables
  protected val parallel: ArrayBuffer[ ArrayBuffer[ Option[ T ] ] ] = new ArrayBuffer()
  adjustParallel() // must be deferred until superclass has finished construction
  // end instance variables

  /**
   * Creates a new spreadsheet with the given name.  The parallel matrix
   * is initialized to be empty, and the spreadsheet will be registered.
   * @param name The name of the spreadsheet
   * @throws SpreadsheetNameException If the name of the spreadsheet is invalid
   */
  def this( name: String ) =
    this( name, true )

  /**
   * Adds a column to the end of the model
   * Overridden in order to keep the parallel matrix in line
   * @param columnName The name of the column
   * @param columnData The data for the column
   */
  override def addColumn( columnName: Object,
			  columnData: Vector[ _ ] ) {
    super.addColumn( columnName,
		     columnData )
    parallel.foreach( _ += None )
  }

  /**
   * Removes a row from the spreadsheet.
   * @param row The row to remove
   */
  override def removeRow( row: Int ) {
    super.removeRow( row )
    parallel.remove( row )
  }

  /**
   * Adds a row to an arbitrary position in the model
   * Overridden in order to keep parallel in line
   * @param rowPos The position at which to insert the row
   * @param rowData The data to put there
   * @throws ArrayIndexOutOfBoundsException If the row was invalid
   */
  override def insertRow( rowPos: Int,
			  rowData: Vector[ _ ] ) {
    super.insertRow( rowPos,
		     rowData )
    val newRow = new ArrayBuffer[ Option[ T ] ]()
    ( 0 to getColumnCount - 1 ).foreach( iteration => newRow += None )
    parallel.insert( rowPos, newRow )
  }

  /**
   * Sets a specific piece of data in the parallel matrix.
   * This will overwrite whatever data is already there
   * @param row The row for the data
   * @param column The column for the data
   * @param data The data to put there
   * @throws IndexOutOfBoundsException If the given position doesn't exist
   */
  def setDataAt( row: Int,
		 column: Int,
		 data: T ) {
    parallel( row )( column ) = Some( data )
  }

  /**
   * Gets the parallel data at the given cell.
   * @param row The row for the data
   * @param column The column for the data
   * @return The data here, or None if there is no data here
   * @throws IndexOutOfBoundsException if the given position doesn't exist
   */
  def getDataAt( row: Int, column: Int ) =
    parallel( row )( column )

  /**
   * Determines if data is at the given position
   * @param row The row for the data
   * @param column The column for the data
   * @return true if there is data here, else false
   * @throws IndexOutOfBoundsException if the given position doesn't exist
   */
  def isDataAt( row: Int, column: Int ) =
    getDataAt( row, column ).isDefined 

  /**
   * Sets the data at the given position, but only if the given position
   * doesn't already have data in it.
   * @param row The row of the data
   * @param column The column of the data
   * @param data The data to put here
   * @return true if the data was inserted, else false.  False means that
   * there was data already here
   * @throws IndexOutOfBoundsException If the given position doesn't exist
   */
  def setDataAtIfEmpty( row: Int,
		        column: Int,
		        data: => T ) = {
    if ( !isDataAt( row, column ) ) {
      setDataAt( row, column, data )
      true
    } else false
  }

  /**
   * Merely updates the data and adjusts the parallel matrix as neccessary
   * @param data The data to put into this sheet
   * @param identifiers The new column identifiers
   */
  override def setDataVector( data: Vector[ _ ],
			      identifiers: Vector[ _ ] ) {
    super.setDataVector( data, identifiers )
    adjustParallel()
  }
  
  /**
   * Adjusts the parallel matrix so that its size is the same
   * as the spreadsheet.
   */
  protected def adjustParallel() {
    if ( parallel != null ) {
      adjustParallelColumns()
      adjustParallelRows()
    }
  }

  /**
   * Adjusts the parallel matrix so it has the same number of columns
   * as the spreadsheet
   */
  protected def adjustParallelColumns() {
    parallel.foreach( row => {
      val numToAdd = getColumnCount - row.length
      if ( numToAdd > 0 ) {
	( 0 to numToAdd - 1 ).foreach( current => row += None )
      } else if ( numToAdd < 0 ) {
	row.reduceToSize( getColumnCount )
      }
    } )
  }

  /**
   * Adjusts the parallel matrix so that it has the same number of
   * rows as the spreadsheet
   */
  protected def adjustParallelRows() {
    val numToAdd = getRowCount - parallel.length
    if ( numToAdd > 0 ) {
      ( 0 to numToAdd - 1 ).foreach( current => {
	val currentRow = new ArrayBuffer[ Option[ T ] ]()
	currentRow ++= ( 0 to getColumnCount - 1 ).map( x => None )
	parallel += currentRow
      } )
    } else if ( numToAdd < 0 ) {
      parallel.reduceToSize( getRowCount )
    }
  }
} // ParallelSpreadsheet

/**
 * A parallel spreadsheet that utilizes lazy instantiation with a
 * special getDataAt function (getInstantiatedDataAt).  If there isn't
 * any data at the given location, then a given function will be used to
 * instantiate it and then return the instantiated data.
 * @param name The name of the spreadsheety
 * @param register Whether or not we should register this spreadsheet
 * @param instantiator What is used to instantiate empty cells.  Takes
 * the row and column of the row that is being instantiated
 * @author Kyle Dewey
 */
class LazyParallelSpreadsheet[ T ]( name: String,
				    register: Boolean,
				    val instantiator: ( Int, Int ) => T ) 
extends ParallelSpreadsheet[ T ]( name, register ) {
  /**
   * Like <code>getDataAt</code>, but it will definitely return T.
   * If there isn't any data at the given location, it will
   * initialize it with instantiator, put the data in, and return
   * the item put in.
   * @param row The row
   * @param column The column
   * @return The data here
   * @throws IndexOutOfBoundsException if the given position doesn't exist
   */
  def getInstantiatedDataAt( row: Int, column: Int ) = {
    if ( !isDataAt( row, column ) ) {
      setDataAt( row, column,
		 instantiator( row, column ) )
    }
    getDataAt( row, column ).get
  }
}
