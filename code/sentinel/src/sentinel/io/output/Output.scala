/*
 * Output.scala
 * 
 * Version:
 *     $Id: Output.scala,v 1.2 2011/05/25 20:04:19 kyledewey Exp $
 *
 * Revisions:
 *      $Log: Output.scala,v $
 *      Revision 1.2  2011/05/25 20:04:19  kyledewey
 *      If null is passed, it is replaced with a null string.
 *
 *      Revision 1.1  2010/07/11 05:40:41  kyledewey
 *      Initial revision
 *
 *
 */

package sentinel.io.output

import java.io._
import sentinel.model._

/**
 * Exception thrown when a format-level error occurred upon writing out
 * a spreadsheet.
 * @param message A message to show the user
 * @author Kyle Dewey
 */
case class SpreadsheetWriteException( message: String ) extends IOException {}

/**
 * Contains helper methods for SpreadsheetWriter.
 * @author Kyle Dewey
 */
object SpreadsheetWriter {
  /**
   * Gets the column names out of a spreadsheet.
   * @param sheet The sheet to get the columns out of
   */
  def columnNames( sheet: Spreadsheet ): Seq[ String ] =
    ( 0 to sheet.getColumnCount - 1 ).map( sheet.getColumnName( _ ) )
}

/**
 * Class that can write out data in a spreadsheet to a file.
 * @author Kyle Dewey
 */
trait SpreadsheetWriter {
  /**
   * Writes out the given data to a file
   * @param data The data to write out
   * @param file The file to write the data to
   * @throws SpreadsheetWriteException If a format-level exception occurred
   * upon writing out the spreadsheet
   * @throws IOException If an error occurred on write
   */
  def writeFile( data: Seq[ Seq[ String ] ], file: File ): Unit
  
  /**
   * Writes out the given spreadsheet to the given file
   * @param sheet The spreadsheet to write out
   * @param file The file to write the spreadsheet to
   * @throws SpreadsheetWriteException If a format-level exception occurred
   * upon writing out the spreadsheet
   * @throws IOException If an error occurred on write
   */
  def writeSpreadsheet( sheet: Spreadsheet, file: File ) {
    var data: Array[ Seq[ String ] ] = new Array( sheet.getRowCount + 1 )
    data( 0 ) = SpreadsheetWriter.columnNames( sheet )
    0.until( sheet.getRowCount ).foreach( rowNum => {
      data( rowNum + 1 ) = sheet.row( rowNum ).get.map( datum =>
	if ( datum != null ) datum.toString else "" )
    })
    writeFile( data, file )
  }

  /**
   * Writes out the given spreadsheet to a file with the given name
   * @param sheet The spreadsheet to write out
   * @param fileName The name of the file to write the spreadsheet to
   * @throws SpreadsheetWriteException If a format-level exception occurred
   * upon writing out the spreadsheet
   * @throws IOException If an error occurred on write
   */
  def writeSpreadsheet( sheet: Spreadsheet, fileName: String ) {
    writeSpreadsheet( sheet,
		      new File( fileName ) )
  }
}
