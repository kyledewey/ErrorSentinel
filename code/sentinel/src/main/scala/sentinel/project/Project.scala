/*
 * Project.scala
 */

package sentinel.project

import sentinel.model._

/**
 * Conatins helper functions for Associations
 * @author Kyle Dewey
 */
object Associations {
  /**
   * If the given sheet name is a replacer spreadsheet, it
   * will call the given function with the corresponding
   * replacer spreadsheet.  Otherwise it will do nothing.
   * @param sheetName The name of the spreadsheet
   * @param function The function to call with the replacer spreadsheet
   * if the given sheetName corresponds to a replacer spreadsheet.  Otherwise
   * does nothing.
   */
  def ifReplacerSpreadsheet( sheetName: String,
			     function: ReplacerSpreadsheet => Unit ) {
    val sheet = ReplacerSpreadsheet.getSpreadsheet( sheetName )
    if ( sheet.isDefined ) {
      function( sheet.get )
    }
  }

  /**
   * Filters for items holding cell ranges.
   * @param items The items holding cell ranges
   * @param toCellRange Function to convert an item to a cell range
   * @param predicate Predicate to test the cell range with
   * @return The items that held cell ranges that matched the predicate.
   */
  def filterCellRanges[ T ]( items: Seq[ T ], toCellRange: T => CellRange, predicate: CellRange => Boolean ): Seq[ T ] = 
    items.filter( ( item: T ) => 
      predicate( toCellRange( item ) ) )

  /**
   * Filters either the good data matchers or error correction pairs based
   * on their cell ranges.
   * @param items Which one to filter
   * @param predicate The predicate to filter with
   * @return items that match
   */
  def filterCellRanges[ T ]( items: Seq[ (T, CellRange) ], predicate: CellRange => Boolean ): Seq[ (T, CellRange) ] =
    filterCellRanges( items, 
		      ( item: (T , CellRange) ) => item._2, 
		      predicate )

  /**
   * Gets all the items that are relevant to the given cell.
   * @param items Which one to filter
   * @param sheet The spreadsheet the cell is in
   * @param row The row of the cell
   * @param column The column of the cell
   * @return The items that are relevant to the given cell.
   */
  def relevantToCell[ T ]( items: Seq[ (T, CellRange) ], 
			   sheet: Spreadsheet,  
			   row: Int,
			   column: Int ) = {
    filterCellRanges( items, _.inRange( sheet, row, column ) )
  }

  /**
   * Gets all the items that have a defined row that is > the given row.
   * If a shift down occurs at the given row, these must be changed.
   * @param items Which one to filter
   * @param row The row to compare against
   * @return All items which have a defined row that is >= the given row
   */
  def relevantToRowInsert[ T ]( items: Seq[ (T, CellRange) ], row: Int ) =
    filterCellRanges( items, range =>
      range.row.isDefined && range.row.get > row )

  /**
   * Like <code>relevantToRowInsert</code>, but it's for columns.
   * @param items Which items to filter
   * @param column The column to compare against
   * @return All items which have a defined column that is > the given column
   */
  def relevantToColumnInsert[ T ]( items: Seq[ (T, CellRange) ], column: Int ) =
    filterCellRanges( items, range =>
      range.column.isDefined && range.column.get > column )
}

/**
 * Container for spreadsheet associations.
 * Associations associate good data matchers with a given cell range,
 * and they also associate error correction matcher/replacer pairs
 * to a given cell range
 * @param _goodDataMatchers The good data matchers, along with the
 * ranges that each matcher is a part of
 * @param _errorCorrectionPairs The error correction pairs, featuring a
 * matcher whose match will trigger the replacer in the pair.  Also
 * holds the cell range that it applies to
 * @author Kyle Dewey
 */
class Associations( private var _goodDataMatchers: Seq[ (Matcher, CellRange) ],
		    private var _errorCorrectionPairs: Seq[ ((Matcher, Replacer), CellRange) ] ) {
  import Associations._

  /**
   * Gets the good data matchers
   * @return The good data matchers
   */
  def goodDataMatchers() =
    _goodDataMatchers

  /**
   * Gets the error correction pairs
   * @return The error correction pairs
   */
  def errorCorrectionPairs() =
    _errorCorrectionPairs

  /**
   * Clears a cell.
   * This simply removes all good data matchers and error correction pairs
   * at the given cell.
   * @param sheet The sheet the cell is in
   * @param row The row the cell is in
   * @param column The column the cell is in
   */
  def clearCell( sheet: ReplacerSpreadsheet, row: Int, column: Int ) {
    sheet.clearGoodData( row, column )
    sheet.clearErrorCorrectionPairs( row, column )
  }
  
  /**
   * Reloads a given cell.
   * This simply clears it and runs <code>cellAdded</code>
   * @param sheet The sheet the cell is in
   * @param row The row of the cell
   * @param column The column of the cell
   */
  def reloadCell( sheet: ReplacerSpreadsheet, row: Int, column: Int ) {
    clearCell( sheet, row, column )
    loadCell( sheet, row, column )
  }

  /**
   * Reloads the given row.
   * @param sheet The sheet holding the row
   * @param row The row to reload
   */
  def reloadRow( sheet: ReplacerSpreadsheet, row: Int ) {
    0.until( sheet.getColumnCount ).foreach( column =>
      reloadCell( sheet, row, column ) )
  }

  /**
   * Reloads the given column.
   * @param sheet The sheet holding the column
   * @param column The column to reload
   */
  def reloadColumn( sheet: ReplacerSpreadsheet, column: Int ) {
    0.until( sheet.getRowCount ).foreach( row =>
      reloadCell( sheet, row, column ) )
  }

  /**
   * Reloads all columns > the given column.
   * @param sheet The sheet holding the columns
   * @param column The last column NOT to reload
   */
  def reloadColumnsAfter( sheet: ReplacerSpreadsheet, column: Int ) {
    ( column + 1 ).until( sheet.getColumnCount ).foreach( column =>
      reloadColumn( sheet, column ) )
  }

  /**
   * Reloads all rows > the given row.
   * @param sheet The sheet holding the rows
   * @param row The last row NOT to reload
   */
  def reloadRowsBelow( sheet: ReplacerSpreadsheet, row: Int ) {
    ( row + 1 ).until( sheet.getRowCount ).foreach( row =>
      reloadRow( sheet, row ) )
  }
						 
  /**
   * Loads good data matchers and error correction pairs
   * appropriately for the cell.  Note that this does not handle
   * any shifting resulting from insertions, etc.
   * @param sheet The replacer spreadsheet
   * @param row The row of the cell
   * @param column The column of the cell
   */
  def loadCell( sheet: ReplacerSpreadsheet,
	        row: Int,
	        column: Int ) {
    relevantToCell( goodDataMatchers, sheet, row, column ).foreach(
      ( goodData: (Matcher, CellRange) ) =>
	sheet.addGoodDataMatcher( row, column, goodData._1 ) )
    relevantToCell( errorCorrectionPairs, sheet, row, column ).foreach(
      ( errorCorrection: ((Matcher, Replacer), CellRange) ) =>
	sheet.addErrorCorrectionPair( row, column, errorCorrection._1 ) )
  }

  /**
   * Reloads all rows at or below the given row.
   * @param sheet The sheet to do the reload on
   * @param row The row for which all rows >= this row will be reloaded
   */
  def reloadRowAtAndBelow( sheet: ReplacerSpreadsheet, row: Int ) {
    reloadRow( sheet, row )
    reloadRowsBelow( sheet, row )
  }

  /**
   * Informs us that the given row has been added to the given sheet.
   * If this is relevant to any of the goodDataMatchers or
   * errorCorrection pairs, then they will be put in the sheet.
   * @param sheet The replacer spreadsheet
   * @param row The number of the row
   */
  def rowAdded( sheet: ReplacerSpreadsheet, row: Int ) {
    reloadRowAtAndBelow( sheet, row )
  }
  
  /**
   * Informs us that the given row has been removed.
   * Note that this assumes that the row has already been removed.
   * @param sheet The spreadsheet which had a row removed
   * @param row The row that was removed
   */
  def rowRemoved( sheet: ReplacerSpreadsheet, row: Int ) {
    if ( sheet.getRowCount > row ) {
      reloadRowAtAndBelow( sheet, row )
    }
  }

  /**
   * Reloads all columns at and after the given column.
   * @param sheet The spreadsheet
   * @param column The starting column to reload
   */
  def reloadColumnsAtAndAfter( sheet: ReplacerSpreadsheet, column: Int ) {
    reloadColumn( sheet, column )
    reloadColumnsAfter( sheet, column )
  }

  /**
   * Informs us that the given column has been added to the given sheet.
   * Loads in good data matchers and error correction pairs
   * appropriately.
   * @param sheet The replacer spreadsheet
   * @param column The number of the column
   */
  def columnAdded( sheet: ReplacerSpreadsheet, column: Int ) {
    reloadColumnsAtAndAfter( sheet, column )
  }

  /**
   * Loads in all good data matchers
   * @pre All cells are clear
   */
  def loadGoodDataMatchers() {
    goodDataMatchers.foreach( goodData => 
      goodData._2.foreach( ( sheetName, row, column ) => 
	ifReplacerSpreadsheet( sheetName,
			       _.addGoodDataMatcher( row, column, goodData._1 ) ) ) )
  }

  /**
   * Loads in all error correction pairs.
   * @pre All cells are clear
   */
  def loadErrorCorrectionPairs() {
    errorCorrectionPairs.foreach( pair =>
      pair._2.foreach( ( sheetName, row, column ) =>
	ifReplacerSpreadsheet( sheetName,
			       _.addErrorCorrectionPair( row, column, pair._1 ) ) ) )
  }

  /**
   * Loads in the given associations.
   * Should be called only once - project initialization time.
   * @pre All spreadsheets have been loaded in
   * @pre All cells in all spreadsheets are clear of Sentinel language
   */
  def loadAssociations() {
    loadGoodDataMatchers()
    loadErrorCorrectionPairs()
  }
}

/**
 * Container for a project
 * @param sheets The sheets associated with this project.  The name is
 * used as a key and the value is the sheet itself.
 * @param langs The languages associated with this project
 * @param associations The associations made between cells in a spreadsheet
 * and parts of the language
 * @param spreadsheetFactory Used to convert the normal spreadsheet
 * implementation to a desired implementation.  Takes the spreadsheet,
 * the project (<code>this</code>), 
 * and whether or not the resulting spreadsheet should be registered.
 * @param register Whether or not to register the spreadsheets
 * @throws ParameterizedInstantiationException If we could not instantiate
 * a replacer
 * @throws SpreadsheetReadException If there was an error in the underlying
 * format
 * @throws FileNotFoundException If the file could not be found
 * @throws SpreadsheetNameException If the name of a spreadsheet is invalid
 * @throws IOException If some other reading error occurred
 * @author Kyle Dewey
 */
class Project[ T <: Spreadsheet ]( var sheets: Map[ String, Sheet ],
				   var langs: Seq[ Language ],
				   val associations: Associations,
				   val spreadsheetFactory: ( Spreadsheet, Project[ T ], Boolean ) => T,
				   val register: Boolean ) {
  // begin constructor
  var spreadsheets = loadSpreadsheets()
  associations.loadAssociations()
  // end constructor

  /**
   * Merely passes false for register
   * @param sheets The sheets to use for the project
   * @param langs The languages associated with this project
   * @param associations The associations made between cells in a spreadsheet
   * and parts of the language
   * @param spreadsheetFactory Used to convert the normal spreadsheet
   * implementation to a desired implementation.  Takes the spreadsheet,
   * the project (<code>this</code>), 
   * and whether or not the resulting spreadsheet should be registered.
   */
  def this( sheets: Map[ String, Sheet ],
	    langs: Seq[ Language ],
	    associations: Associations,
	    spreadsheetFactory: ( Spreadsheet, Project[ T ], Boolean ) => T ) =
	      this( sheets, langs, associations, spreadsheetFactory, false )
  
  /**
   * Runs the given function against all spreadsheets.
   * @param function The function to run against all spreadsheets
   */
  def foreachSpreadsheet( function: T => Unit ) {
    spreadsheets.values.foreach( function( _ ) )
  }

  /**
   * Loads in the given sheet.
   * @param sheet The sheet to load in
   * @return The loaded in spreadsheet
   * @throws SpreadsheetReadException If there was an error in the underlying
   * format.
   * @throws FileNotFoundException If the file could not be found
   * @throws SpreadsheetNameException If the name of a spreadsheet is invalid
   * @throws IOException If some other reading error occurred
   */
  def loadSpreadsheet( sheet: Sheet ) = {
    val retval =
      spreadsheetFactory( SheetReader.readSpreadsheet( sheet.name,
						       true,
						       sheet.fileName,
						       sheet.fileType ), 
		          this, 
		          register )
    if ( register ) {
      Spreadsheet.registerSpreadsheet( retval )
    }
    retval
  }

  /**
   * Loads in all sheets associated with the project.
   * Note that it is expected that calling code will perform error correction.
   * Only data is loaded in; no replacers are set.
   * @return All the spreadsheets that are associated with this project.  The
   * name is the key and the value is the sheet itself
   * @throws SpreadsheetReadException If there was an error in the underlying
   * format.
   * @throws FileNotFoundException If the file could not be found
   * @throws SpreadsheetNameException If the name of a spreadsheet is invalid
   * @throws IOException If some other reading error occurred
   */
  def loadSpreadsheets(): Map[ String, T ] =
    Map() ++ sheets.values.map( sheet =>
      (sheet.name, loadSpreadsheet( sheet )) )
}

import javax.swing._
import javax.swing.filechooser._
import java.io._

/**
 * A file chooser that recognizes project files.
 * @author Kyle Dewey
 */
object ProjectFileChooser extends javax.swing.filechooser.FileFilter {
  /**
   * Matches any files with an xml extension.
   * @param file The file to check
   * @return true if the file has an XML extension, else false
   */
  def accept( file: File ) =
    file.getName.toLowerCase.endsWith( ".xml" )
  
  /**
   * Returns a description string for projects.
   * @return "Project Files"
   */
  def getDescription() =
    "Project Files"
}
