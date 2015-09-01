/*
 * CSV.scala
 *
 * Version:
 *     $Id: CSV.scala,v 1.2 2011/06/17 19:51:14 kyledewey Exp kyledewey $
 *
 * Revisions:
 *      $Log: CSV.scala,v $
 *      Revision 1.2  2011/06/17 19:51:14  kyledewey
 *      Made the toToken() routine far more efficient.
 *
 *      Revision 1.1  2010/07/11 05:41:08  kyledewey
 *      Initial revision
 *
 *
 */

package sentinel.io.output.csv

import java.io._
import sentinel.io.input.csv._
import sentinel.io.output._

/**
 * Writes spreadsheet data to a csv file.
 * @author Kyle Dewey
 */
class WriteCSV( val delim: Char, val quote: Char ) extends SpreadsheetWriter {
  /**
   * Creates a new CSV writer using the default delimiter and quote
   */
  def this() =
    this( CSV.DELIMITER, CSV.QUOTE )
  
  /**
   * Given a datum, will convert it to a writable token
   * @param datum The datum to convert
   * @return A writable token
   * @throws SpreadsheetWriteException If the token could not be made
   */
  def toToken( datum: String ) = {
    val hasDelim = datum.indexOf( delim ) != -1
    val hasQuote = datum.indexOf( quote ) != -1
    
    if ( hasDelim && 
	 hasQuote ) {
      throw new SpreadsheetWriteException( "Data contains both the delimiter " +
					   delim + " and the quote " +
					   quote + "; cannot write it out." )
    } else if ( hasDelim ) {
      "" + quote + datum + quote

    } else {
      datum
    }
  }

  /**
   * Writes out the data to a file.
   * @param data The data to write out
   * @param file The file to write the data to
   * @throws SpreadsheetWriteException If we couldn't convert any portion
   * of the data to CSV
   * @throws IOException If an error occurred on write
   */
  def writeFile( data: Seq[ Seq[ String ] ], file: File ) {
    val output = new BufferedWriter( new FileWriter( file ) )
    val numCols = data( 0 ).length
    var rowPos = 0
    var colPos = 0
    try {
      data.foreach( rowData => {
	var outputColumn = new Array[ String ]( numCols )
	colPos = 0
	rowPos += 1
	rowData.foreach( cell => {
	  val safeCell = if ( cell != null ) cell else ""
	  colPos += 1
	  outputColumn( colPos - 1 ) = toToken( safeCell )
	} )
	output.write( outputColumn.mkString( delim + "" ) )
	output.newLine()
      } )
      output.close()
    } catch {
      case e: SpreadsheetWriteException =>
	output.close()
	throw new SpreadsheetWriteException( "Row: " + rowPos +
					     " Column: " + colPos + ": " +
					     e.getMessage )
    }
  } // writeFile
} // WriteCSV

/**
 * A CSV writer for tab-delimited data.
 * Merely uses a tab instead of a comma.
 * @author Kyle Dewey
 */
class WriteTabDelimited extends WriteCSV( '\t', CSV.QUOTE ) {}
