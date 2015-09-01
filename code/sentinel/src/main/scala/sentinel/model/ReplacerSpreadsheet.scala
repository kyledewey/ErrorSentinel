/*
 * ReplacerSpreadsheet.scala
 */

package sentinel.model

import java.util.Vector
import javax.swing.table.TableModel

/**
 * Contains helper methods and convenience methods for ReplaceSpreadsheet
 * @author Kyle Dewey
 */
object ReplacerSpreadsheet {
  import Spreadsheet._

  /**
   * Creates a new ReplacerSpreadheet with the given name
   * @param name The name of the spreadsheet
   * @param register If we should register it or not
   * @return A new, empty spreadsheet
   * @throws SpreadsheetNameException If the name of the spreadsheet isn't
   * valid
   */
  def apply( name: String, register: Boolean ) =
    DefaultReplacerSpreadsheet( name, register )

  /**
   * Creates a new ReplacerSpreadsheet from another spreadsheet.
   * Note that error correction is NOT performed.
   * @param sheet The sheet to promote from
   * @param register If we should register or not
   * @return A new ReplacerSpreadsheet, containing all the information in
   * the old spreadsheet.  Also returns a parallel matrix of error
   * correction results, but only if error correction is on
   * @throws SpreadsheetNameException If the name of the spreadsheet isn't
   * valid
   */
  def apply( sheet: Spreadsheet, 
	     register: Boolean ): DefaultReplacerSpreadsheet[ NonDataCellContents ] = 
    Spreadsheet.copyContents( sheet, apply( sheet.name, register ) )

  /**
   * Like CellRange's foreach, but it only considers ReplacerSpreadsheets.
   * @param cellRange The cell range to go over
   * @param function The function to call on the given range
   */
  def foreach( cellRange: CellRange,
	       function: ( ReplacerSpreadsheet, Int, Int ) => Unit ) {
    cellRange.foreach( ( sheet, row, column ) => {
      val spreadsheet = getSpreadsheet( sheet )
      if ( spreadsheet.isInstanceOf[ ReplacerSpreadsheet ] ) {
	function( spreadsheet.asInstanceOf[ ReplacerSpreadsheet ],
		  row, column )
      }
    })
  }

  /**
   * Sets all spreadsheets in the given range to have the given
   * "Good Data" matcher
   * Note that any sheets must be registered.
   * @param range The cell range to execute over
   * @param matcher The matcher to add
   */
  def addGoodDataMatcher( range: CellRange,
			  matcher: Matcher ) {
    foreach( range,
	     ( sheet, row, column ) =>
	       sheet.addGoodDataMatcher( row, column, matcher ) )
  }
    
  /**
   * Sets all spreadsheets in the given range to have the given
   * "Error Correction" matcher/replacer pair
   * @param range The cell range to execute over
   * @param pair The pair to add
   */
  def addErrorCorrectionPair( range: CellRange,
			      pair: (Matcher, Replacer) ) {
    foreach( range,
	     ( sheet, row, column ) =>
	       sheet.addErrorCorrectionPair( row, column, pair ) )
  }

  /**
   * Gets the given replacer spreadsheet, if it exists.
   * @param name The name of the replacer spreadsheet
   * @return The replacer spreadsheet, or None if the sheet doesn't
   * exist or if the sheet is not a replacer spreadsheet
   */
  def getSpreadsheet( name: String ) = {
    val sheet = Spreadsheet.getSpreadsheet( name )
    if ( sheet.isDefined &&
	 sheet.get.isInstanceOf[ ReplacerSpreadsheet ] ) {
      Some( sheet.get.asInstanceOf[ ReplacerSpreadsheet ] )
    } else None
  }
}

/**
 * Describes a table model that can optionally choose to put a value
 * in for a given position.
 * @author Kyle Dewey
 */
trait ConditionalSpreadsheet extends Spreadsheet {
  /**
   * Temporarily sets a value at the given position, and tests it against
   * any error correction facilities for the cell.  Note that this will
   * not actually set the value; once the call completes the value will
   * have reverted to the original value.
   * @param item The item to put here
   * @param row The row to set at
   * @param column The column to set at
   * @return An instance result describing what happened
   * @throws ArrayIndexOutOfBoundsException If the given row, column pair
   * points to an invalid location in the spreadsheet
   */
  def tryValueAt( item: String, row: Int, column: Int ): Option[ InstanceResult ]
}

/**
 * Represents a spreadsheet that has matchers and replacers in it.
 * There are two important concepts here: the "Good Data" matchers and
 * "Error Correction" matcher/replacer pairs.
 * <p>A cell can have 0-N "Good Data" matchers associated with it.  These
 * are used to validate that data is valid.  If all return true, then
 * the data is considered valid.  If there are none associated, the data is
 * considered valid.  If at least one returns false, the data is considered
 * erroneous.  Note tht the behavior is short circuiting: the first that
 * returns false will prevent the others from being executed. Also
 * note that exceptions are considered the same as false values.</p>
 * <p>A cell can have 0-N "Error Correction" matcher/replacer pairs. If
 * all the "Good Data" matchers return true, these are irrelevant.  If not,
 * execution passes to these.  These are run through, one at a time, executing
 * the matchers.  For the first matcher that matches, the matcher's
 * associated replacer is executed.  That replacer's returned result is
 * intended to be used as an error correction value. As to how exactly
 * that value is used depends on whether or not this is run interactively,
 * so at this level of the class hierarchy nothing is done beyond returning
 * that result.  If the replacer throws an exception, it is considered an
 * uncorrectable error.  If the "Good Data" matchers had at least one failure,
 * and there are no "Error Correction" matcher/replacer pairs, it is considered
 * an uncorrectable error.  If the "Good Data" matchers had at least one
 * failure and no "Error Correction" matchers matched, it is considered
 * an uncorrectable error. Note that if a matcher throws an exception,
 * it is considered to have returned false.</p>
 * @author Kyle Dewey
 */
trait ReplacerSpreadsheet extends ConditionalSpreadsheet {
  import InstanceResult._

  /**
   * Adds a "Good Data" matcher to the given cell.
   * If the matcher is already associated, this is a no-op
   * @param row The row for the matcher
   * @param column The column for the matcher
   * @param matcher The matcher to put here
   * @throws IndexOutOfBoundsException If the given position doesn't exist
   */
  def addGoodDataMatcher( row: Int, column: Int, matcher: Matcher ): Unit

  /**
   * Gets all the "Good Data" matchers for the given cell.
   * Returns them in the order that they were put in there.
   * @param row The row for the matcher
   * @param column The column for the matcher
   * @return All the "Good Data" matchers at this cell
   * @throws IndexOutOfBoundsException If the given position doesn't exist
   */
  def getGoodDataMatchers( row: Int, column: Int ): Seq[ Matcher ]

  /**
   * Adds an "Error Correction" matcher, replacer pair to the given cell.
   * If it is already associated, this is a no-op.
   * @param row The row
   * @param column The column
   * @param pair The matcher/replacer pair
   * @throws IndexOutOfBoundsException If the given position doesn't exist
   */
  def addErrorCorrectionPair( row: Int, 
			      column: Int, 
			      pair: (Matcher, Replacer) ): Unit

  /**
   * Gets all "Error Correction" matcher, replacer pairs associated with
   * the given cell.  Gets them in the order they were added.
   * @param row The row
   * @param column The columm
   * @returns All "Error Correction" matcher/replacer pairs associated with
   * the given cell, in the order which they were associated
   * @throws IndexOutOfBoundsException If the given position doesn't exist
   */
  def getErrorCorrectionPairs( row: Int, 
			       column: Int ): Seq[ (Matcher, Replacer) ]

  /**
   * Gets all instances directly connected to the given cell.
   * This means all good data matchers, and all matcher/replacer
   * error correction pairs.
   * @param row The row
   * @param column The column
   * @return All instances here
   */
  def getInstances( row: Int, column: Int ): Seq[ Instance ] =
    getGoodDataMatchers( row, column ) ++ getErrorCorrectionPairs( row, column ).flatMap( pair => Seq( pair._1, pair._2 ) )

  /**
   * Clears out good data matchers from the given cell.
   * @param row The row of the cell
   * @param column The column of the cell
   */
  def clearGoodData( row: Int, column: Int ): Unit

  /**
   * Clears out the error correction pairs from the given cell
   * @param row The row of the cell
   * @param column the column of the cell
   */
  def clearErrorCorrectionPairs( row: Int, column: Int ): Unit

  /**
   * Executes all "Good Data" matchers at the given row and column.
   * Returns an InstanceResult describing what happened.  Note that
   * throwing an exception is considered the same as being false
   * @param row The row
   * @param column The column
   * @return An InstanceResult describing what happened.
   * @throws IndexOutOfBoundsException If the given position doesn't exist
   */
  def executeGoodData( row: Int, column: Int ): InstanceResult = 
    SentinelHelpers.mapFind( getGoodDataMatchers( row, column ),
			     ( m: Matcher ) => executeMatcher( m ),
			     ( i: InstanceResult ) => i.failure )
                   .getOrElse( new InstanceSuccess() )

  /**
   * Executes any error correction pairs for the given row and column.
   * Returns an InstanceResult describing what happened for the replacer
   * executed on the first matching matcher, or None if none of the
   * error correction pairs matched.
   * @param row The row
   * @param column The column
   * @return An InstanceResult describing what happened for the first matching
   * replacer, or None if none of the replacers matched
   * @throws IndexOutOfBoundsException If the given position doesn't exist
   */
  def executeErrorCorrectionPairs( row: Int, column: Int ) =
    SentinelHelpers.mapFind( getErrorCorrectionPairs( row, column ),
			     ( p: (Matcher, Replacer) ) =>
			       executeErrorCorrectionPair( p ),
			     ( result: Option[ InstanceFailure[ Replacer ] ] ) =>
			       result.isDefined )
                   .getOrElse( None )
  
  /**
   * Sets the value at the given position, and runs it through
   * the "GoodData" matchers and the "Error Correction" matcher/replacer
   * pairs.
   * @param value The value to try to put here
   * @param row The row to put it at
   * @param column The column to put it at
   * @return One of the following: <table border="1">
   * <tr><th>Return Value</th><th>Meaning</th></tr>
   * <tr><td>Some( InstanceSuccess )</td><td>The data was considered valid by the
   * "Good Data" matchers</td></tr>
   * <tr><td>Some( InstanceFailureException )</td><td>The data was considered invalid,
   * and the replacer for the first matching error correction pair threw an exception
   * </td></tr>
   * <tr><td>Some( InstanceFailureReplacement )</td><td>The data was considered invalid,
   * and the replacer for the first matching error correction pair returned a replacement
   * </td></tr>
   * <tr><td>None</td><td>The data was considered invalid, but no error correction pair
   * matched</td></tr></table>
   * @throws ArrayIndexOutOfBoundsException If the given row, column pair
   * points to an invalid location in the spreadsheet
   */
  def tryValueAt( value: String, row: Int, column: Int ) = {
    val oldValue = getValueAt( row, column )
    setValueAt( value, row, column )
    updateCurrent( row, column )
    val goodDataResult = executeGoodData( row, column )
    val retval =
      if ( goodDataResult.success ) {
	Some( goodDataResult )
      } else {
	executeErrorCorrectionPairs( row, column )
      }
    setValueAt( oldValue, row, column )
    retval
  }
} // ReplacerSpreadsheet

import scala.collection.mutable.ArrayBuffer

/**
 * Simple container class for simplifying <code>DefaultReplacerSpreadsheet</code>.
 * Holds all the non-data for each cell.
 * @param goodDataMatchers The "Good Data" matchers to use for a cell
 * @param errorCorrectionPairs The "Error Correction" matcher/replacer pairs for a cell
 * @author Kyle Dewey
 */
class NonDataCellContents( val goodDataMatchers: SeqSet[ Matcher ],
			   val errorCorrectionPairs: SeqSet[ (Matcher, Replacer) ] ){
  /**
   * Creates a new contents that doesn't actually hold anything
   */
  def this() =
    this( new SeqSet(),
	  new SeqSet() )

  /**
   * Adds a good data matcher to this
   * If it's a repeat, this is a no-op
   * @param matcher The matcher to add
   */
  def addGoodData( matcher: Matcher ) {
    goodDataMatchers.addItem( matcher )
  }

  /**
   * Adds an error correction pair to this
   * @param pair The pair to add
   */
  def addErrorCorrection( pair: (Matcher, Replacer) ) {
    errorCorrectionPairs.addItem( pair )
  }

  /**
   * Clears the good data.
   */
  def clearGoodData() {
    goodDataMatchers.clear()
  }

  /**
   * Clears the error correction pairs
   */
  def clearErrorCorrection() {
    errorCorrectionPairs.clear()
  }
}

/**
 * Contains routines for making DefaultReplacerSpreadsheets
 * @author Kyle Dewey
 */
object DefaultReplacerSpreadsheet {
  /**
   * Creates a new default replacer spreadsheet.
   * @param name The name of the spreadsheet
   * @param register Whether or not to register the spreadsheet
   * @param instantiator What to use for lazy instantiation of cells
   * @return A new DefaultReplacerSpreadsheet made with the given params
   */
  def apply[ T <: NonDataCellContents ]( name: String, register: Boolean, instantiator: ( Int, Int ) => T ): DefaultReplacerSpreadsheet[ T ] = 
    new DefaultReplacerSpreadsheet[ T ]( name, register, instantiator )

  /**
   * Creates a new default replacer spreadsheet that
   * uses the base class of NonDataCellContents.
   * @param name The name of the spreadsheet
   * @param register Whether or not to register the spreadsheet
   * @return A new DefaultReplacerSpreadsheet that uses the given params
   * and uses the base class of NonDataCellContents
   */
  def apply( name: String, register: Boolean ): DefaultReplacerSpreadsheet[ NonDataCellContents ] =
    apply( name, register, ( row: Int, column: Int ) => new NonDataCellContents() )
}
					   
/**
 * Implementation of ReplacerSpreadsheet that maintains a parallel matrix of
 * "Good Data" matchers and "Error Correction" 
 * @param name The name of the spreasheet
 * @param register If we should register this spreadsheet
 * @param instantiator Creates a new empty NonDataCellContents object
 * on demand
 * @throws SpreadsheetNameException if the name of the spreadshet is
 * invalid
 * @author Kyle Dewey
 */
class DefaultReplacerSpreadsheet[ T <: NonDataCellContents ]
( name: String, register: Boolean, instantiator: ( Int, Int ) => T )
extends LazyParallelSpreadsheet[ T ]( name, register, instantiator ) with ReplacerSpreadsheet {
  /**
   * Adds a "Good Data" matcher to the given cell.
   * If the matcher is already associated, this is a no-op
   * @param row The row for the matcher
   * @param column The column for the matcher
   * @param matcher The matcher to put here
   * @throws IndexOutOfBoundsException If the given position doesn't exist
   */
  def addGoodDataMatcher( row: Int, column: Int, matcher: Matcher ) {
    getInstantiatedDataAt( row, column ).addGoodData( matcher )
  }

  /**
   * Gets all the "Good Data" matchers for the given cell.
   * Returns them in the order that they were put in there.
   * @param row The row for the matcher
   * @param column The column for the matcher
   * @return All the "Good Data" matchers at this cell
   * @throws IndexOutOfBoundsException If the given position doesn't exist
   */
  def getGoodDataMatchers( row: Int, column: Int ) =
    getInstantiatedDataAt( row, column ).goodDataMatchers.asSeq

  /**
   * Adds an "Error Correction" matcher, replacer pair to the given cell.
   * If it is already associated, this is a no-op.
   * @param row The row
   * @param column The column
   * @param pair The matcher/replacer pair
   * @throws IndexOutOfBoundsException If the given position doesn't exist
   */
  def addErrorCorrectionPair( row: Int, 
			      column: Int, 
			      pair: (Matcher, Replacer) ) {
    getInstantiatedDataAt( row, column ).addErrorCorrection( pair )
  }

  /**
   * Gets all "Error Correction" matcher, replacer pairs associated with
   * the given cell.  Gets them in the order they were added.
   * @param row The row
   * @param column The columm
   * @returns All "Error Correction" matcher/replacer pairs associated with
   * the given cell, in the order which they were associated
   * @throws IndexOutOfBoundsException If the given position doesn't exist
   */
  def getErrorCorrectionPairs( row: Int, column: Int ) =
    getInstantiatedDataAt( row, column ).errorCorrectionPairs.asSeq

  /**
   * Clears out good data matchers from the given cell.
   * @param row The row of the cell
   * @param column The column of the cell
   */
  def clearGoodData( row: Int, column: Int ) {
    getInstantiatedDataAt( row, column ).clearGoodData()
  }

  /**
   * Clears out the error correction pairs from the given cell
   * @param row The row of the cell
   * @param column the column of the cell
   */
  def clearErrorCorrectionPairs( row: Int, column: Int ) {
    getInstantiatedDataAt( row, column ).clearErrorCorrection()
  }
} // DefaultReplacerSpreadsheet
