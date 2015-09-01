/*
 * Input.scala
 *
 * Version:
 *     $Id: Input.scala,v 1.2 2010/07/17 02:06:24 kyledewey Exp $
 *
 * Revisions:
 *      $Log: Input.scala,v $
 *      Revision 1.2  2010/07/17 02:06:24  kyledewey
 *      readSpreadsheet() now uses Spreadsheet's apply() instead
 *      of calling a constructor for spreadsheet directly.
 *
 *      Revision 1.1  2010/07/11 05:39:26  kyledewey
 *      Initial revision
 *
 *
 */

package sentinel.io.input

import java.io._
import sentinel.model._

/**
 * Exception thrown when an error occurred upon reading in a spreadsheet.
 * @param message A message to show the user
 * @author Kyle Dewey
 */
case class SpreadsheetReadException( message: String ) extends IOException {}

/**
 * Contains helper methods for SpreadsheetReader
 * @author Kyle Dewey
 */
object SpreadsheetReader {
  /**
   * Gets the max number of rows and columns in the given data
   * @param data The data to get the max rows and columns of
   * @return The max number of rows and columns
   */
  def rowsColumnsCount( data: Seq[ Seq[ _ ] ] ) = {
    val maxCols = data.map( _.size ).foldLeft( 0 )( Math.max( _, _ ) )
    ( data.size,
      maxCols )
  }
}

/**
 * Class that can read in data from a file and return a spreadsheet.
 * The class will convert the data to a series of rows and columns.
 * @author Kyle Dewey
 */
trait SpreadsheetReader {
  /**
   * Given a file, it will read in the data as rows
   * and columns.
   * @param file The file to read in
   * @return The data in the file
   * @throws SpreadsheetReadException If there was an error in the underlying
   * format
   * @throws FileNotFoundException If the file could not be found
   * @throws IOException If some other reading error occurred
   */
  def readFile( file: File ): Seq[ Seq[ String ] ]

  /**
   * Given the name of a spreadsheet and a file, it will
   * read in the data and return a new spreadsheet containing
   * the data.
   * @param name The name of the spreadsheet to create
   * @param file The file to read in
   * @param columnNames If the first row should be treated as column names
   * @return A new spreadsheet containing the read in data
   * @throws SpreadsheetReadException If there was an error in the underlying
   * format
   * @throws FileNotFoundException If the file could not be found
   * @throws SpreadsheetNameException If the name of a spreadsheet is invalid
   * @throws IOException If some other reading error occurred
   */
  def readSpreadsheet( name: String, 
		       file: File, 
		       columnNames: Boolean ): Spreadsheet = {
    val data = readFile( file )
    val ( rowCount,
	  columnCount ) = SpreadsheetReader.rowsColumnsCount( data )
    if ( rowCount == 0 ||
	 columnCount == 0 ) {
      throw new SpreadsheetReadException( "File is empty: " + file.getName )
    }
    
    val retval = Spreadsheet( name )

    // create the columns, followed by the rows
    if ( columnNames ) {
      val asList = data.toList
      asList.head.foreach( ( columnName: String ) => 
	retval.addColumn( Some( columnName ) ) )
      asList.tail.foreach( ( column: Seq[ String ] ) =>
	retval.addRow( Some( column ) ) )
    } else {
      ( 1 to columnCount ).foreach( retval.addColumn )
      data.foreach( ( column: Seq[ String ] ) =>
	retval.addRow( Some( column ) ) )
    }

    retval
  }

  /**
   * Given the name of a spreadsheet and a name of a file, it will
   * read in the data and return a new spreadsheet containing the data.
   * @param name The name of the spreadsheet to create
   * @param fileName The name of the file to read in
   * @param columnNames If the first row should be treated as column names
   * @return A new spreadsheet containing the read in data
   * @throws SpreadsheetReadException If there was an error in the underlying
   * format
   * @throws FileNotFoundException If the file could not be found
   * @throws SpreadsheetNameException If the name of a spreadsheet is invalid
   * @throws IOException If some other reading error occurred
   */
  def readSpreadsheet( name: String,
		       fileName: String,
		       columnNames: Boolean ): Spreadsheet = {
    readSpreadsheet( name,
		     new File( fileName ),
		     columnNames )
  }
}
