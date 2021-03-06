/*
 * CSV.scala
 */

package sentinel.io.input.csv

import sentinel.io.input._
import java.io._

/**
 * Contains constants for CSV parsers.
 * @author Kyle Dewey
 */
object CSV {
  // begin constants
  val DELIMITER = ','
  val QUOTE = '"'
  // end constants
}
  
/**
 * Reads in data from a CSV file.  The separator and quote characters
 * can be anything.
 * @param delim The delimiter character to use.  Defaults to DELIMITER
 * @param quote The quote character to use.  Defaults to QUOTE
 * @author Kyle Dewey
 */
class ParseCSV( val delim: Char, val quote: Char ) extends SpreadsheetReader {
  /**
   * Creates a new CSV parser, using the default characters for
   * quotes and delimiters
   */
  def this() =
    this( CSV.DELIMITER,
	  CSV.QUOTE )

  /**
   * Verifies that the character at the given position within the given
   * string is either beyond the string or a delimiter.
   * @param line The line to verify
   * @param pos The position to verify
   * @throws SpreadsheetReadException If the character at the given position
   * is not beyond the line and it's not a delimiter
   */
  def verifyNext( line: String, pos: Int ) {
    if ( pos < line.length &&
	 line.charAt( pos ) != delim ) 
	   throw new SpreadsheetReadException( "Expected delimiter at " +
					       "position " + pos + "; " +
					       "found " + line.charAt( pos ) )
  }

  /**
   * Parses a quoted token.
   * @param line The line to parse
   * @param start The starting position; one character after the quote
   * @return The token and the new starting position; The start is one
   * past the quote, and will be either end of line or a delimiter
   * @throws SpreadsheetReadException If an unterminated quoted string was
   * found
   */
  def parseQuotedToken( line: String, start: Int ): (String, Int) = {
    var end = start
    val length = line.length
    while( end < length &&
	   line.charAt( end ) != quote ) {
      end += 1
    }
    if ( end >= length ) {
      throw new SpreadsheetReadException( "Unterminated quoted string" )
    } else {
      (line.substring( start, end ), end + 1)
    }
  }

  /**
   * Parses an unquoted token
   * @param line The line to parse
   * @param start The starting position; the first character of the token
   * @return The token and the new starting position.  The new start will
   * either be end of line or a delimiter.
   */
  def parseUnquotedToken( line: String, start: Int ): (String, Int) = {
    var end = start
    val length = line.length
    while( end < length &&
	   line.charAt( end ) != delim ) {
      end += 1
    }
    (line.substring( start, end ), end)
  }

  /**
   * Parses a token on a line.  A token is a single cell.
   * @param line The line to parse
   * @param start The starting position to parse at
   * @return The token, and the new starting position to parse at.  The new
   * starting position will either be the end of the line or a delimiter
   * @throws SpreadsheetReadException If we started on a non-delimiter
   */
  def parseToken( line: String, start: Int ) = {
    try {
      if ( line.charAt( start ) == quote ) {
	parseQuotedToken( line, start + 1 )
      } else {
	parseUnquotedToken( line, start )
      }
    } catch {
      case e: IndexOutOfBoundsException =>
	throw new SpreadsheetReadException( "Expected token, but found end " +
					    "of line" )
    }
  }
    
  /**
   * Parses a given line.
   * @param line The line to parse
   * @return The datums on the line
   * @throws SpreadsheetReadException If there was an error in the underlying
   * format
   */
  def parseLine( line: String ): Seq[ String ] = {
    var retval: Seq[ String ] = Seq()

    // will get a tail recursion optimization
    def parseLine( start: Int ) {
      if ( start < line.length ) {
	val ( token,
	      newStart ) = parseToken( line,
				       start )
	retval ++= Seq( token )
	verifyNext( line, newStart )
	parseLine( newStart + 1 )
      }
    }
    parseLine( 0 )
    retval
  }

  /**
   * Reads in the given file
   * @param file The file to read in
   * @return The data in the file
   * @throws SpreadsheetReadException If there was an error in the underlying
   * format
   * @throws FileNotFoundException If the file could not be opened
   * @throws IOException If some other read error occurred
   */
  override def readFile( file: File ) = {
    import java.util.Scanner
    var retval: Seq[ Seq[ String ] ] = Seq()
    val input = new Scanner( file )
    var lineNum = 0

    try {
      while( input.hasNextLine ) {
	lineNum += 1
	retval ++= Seq( parseLine( input.nextLine ) )
      }
      input.close()
      retval
    } catch {
      case e: SpreadsheetReadException => {
	input.close()
	throw new SpreadsheetReadException( "Line " + lineNum + ": " + 
					    e.getMessage )
      }
      case e: IOException => {
	input.close()
	throw e
      }
    }
  }
}

/**
 * A CSV parser for tab-delimited data.
 * Merely uses a tab instead of a comma.
 * @author Kyle Dewey
 */
class ParseTabDelimited extends ParseCSV( '\t', CSV.QUOTE ) {}

import javax.swing.filechooser.{ FileFilter => SFileFilter }

/**
 * A file filter that recognizes CSV files.
 * @author Kyle Dewey
 */
object CSVFilter extends SFileFilter {
  /**
   * Accepts files that end with ".csv".
   * @param file The file to test
   * @return true if its name ends with ".csv"
   */
  def accept( file: File ) =
    file.getName.toLowerCase.endsWith( ".csv" )

  /**
   * Gets a description for CSV files
   * @return A description for CSV files
   */
  def getDescription() =
    "Comma-separated value (CSV) files."
}

/**
 * A file filter that recognizes tab-delimited files.
 * @author Kyle Dewey
 */
object TabDelimitedFilter extends SFileFilter {
  /**
   * Accepts files that end with ".tab".
   * @param file The file to test
   * @return true if its name ends with ".tab"
   */
  def accept( file: File ) =
    file.getName.toLowerCase.endsWith( ".tab" )

  /**
   * Gets a description for tab-delimited files
   * @return A description for tab-delimited files
   */
  def getDescription() =
    "Tab-delimited files"
}
