/*
 * RuleConverter.scala
 */

package sentinel.utils.legacy

import sentinel.model._
import sentinel.project.WriteXML._
import scala.xml._

/**
 * Holds routines common for both GoodDataParser and ErrorCorrectionParser.
 * @author Kyle Dewey
 */
object ParserCommon {
  /**
   * Strips out the given values from a seq.
   * @param seq The seq to strip from
   * @param which Which items to strip out
   * @return The stripped out items.  Repeats are allowed
   */
  def stripOut[ T ]( seq: Seq[ T ], which: Seq[ Int ] ): Seq[ T ] =
    which.map( seq( _ ) )

  /**
   * Like <code>stripOut</code>, but it works with a set.  Note that
   * the order is at the mercy of the set's iterator.
   * @param seq The seq to strip from
   * @return The stripped out items.
   */
  def stripOut[ T ]( seq: Seq[ T ], which: Set[ Int ] ): Seq[ T ] =
    stripOut( seq, which.toSeq )

  /**
   * Makes sure that all the given items are not null strings
   * @param items the items to check
   * @return true if they are all non-null strings, else false
   */
  def allDefined( items: Seq[ String ] ) =
    items.forall( _.length > 0 )

  /**
   * Makes sure all the given items are null strings
   * @param items the items to check
   * @return true if they are all null strings, else false
   */
  def allUndefined( items: Seq[ String ] ) =
    items.forall( _.length == 0 )

  /**
   * Strips the start end end quotes off of an item, but
   * only if it has double quotes in both positions.
   * @param item The item to strip the quotes of
   * @return Either the same item, or the item with quotes, depending
   * on whether or not it was quoted
   */
  def stripQuotes( item: String ) =
    if ( item.length >= 2 &&
	 item.charAt( 0 ) == '"' &&
	 item.charAt( item.length - 1 ) == '"' ) 
      item.substring( 1, item.length - 1 )
    else item

  /**
   * Give a sheet and a column, it will convert it to an XML cell
   * range.  The row will be for the current row
   * @param sheet The spreadsheet we are referring to
   * @param column The column of the spreadsheet
   * @return The XML cell range node
   */
  def toCellRange( sheet: String, column: Int ) = 
    cellRangeToNode( new CellRange( Some( sheet ), 
				    None, 
				    Some( column ) ) )

  /**
   * Converts a value to a named parameter.
   * String will be used for the type
   * @param name The name of the value
   * @param contents The contents of the named parameter
   */
  def valueToNamedParam( name: String, contents: String ) =
    new NamedParam( name, Constant( contents ) )

  /**
   * Converts a variable to a named parameter.
   * String will be used for the type
   * @param name The name of the variable
   * @param sheet The spreadsheet the variable is from
   * @param column The column the variable is from
   */
  def variableToNamedParam( name: String, sheet: String, column: Int ) =
    new NamedParam( name, 
		    new SpreadsheetVariable( Some( sheet ),
					     None,
					     Some( column ) ) )

  /**
   * Makes a values node, given name/contents pairs.
   * @param pairs The name/contents pairs
   * @return An XML values node holding all the given values information
   */
  def toValues( pairs: Seq[ (String, String) ] ) = 
    namedParamsToValuesNode( pairs.map( pair => 
      valueToNamedParam( pair._1, pair._2 ) ) )

  /**
   * Makes a variables node, given name/sheet/column tuples.
   * @param tuples The name/sheet/column tuples
   * @return An XML variables node holding all the given variables information
   */
  def toVariables( tuples: Seq[ Tuple3[ String, String, Int ] ] ) = 
    namedParamsToVariablesNode( tuples.map( tuple =>
      variableToNamedParam( tuple._1, tuple._2, tuple._3 ) ) )

  /**
   * Converts the given instance information to an instance node.
   * Note that this duplicates a bit of functionality in project's
   * Writer. This avoids having to load in the language and create instances
   * that will never actually be used.
   * @param className The name of the class
   * @param values Pairs decribing value name/contents
   * @param variables Tuples describing variable name/sheet/column
   * @return The inner part of an instance node (either matcher or replacer)
   */
  def instanceInside( className: String, 
		      values: Seq[ (String, String) ],
		      variables: Seq[ Tuple3[ String, String, Int ] ] ) = 
    Seq( classNameToNode( className ),
	 toValues( values ),
	 toVariables( variables ) )

  /**
   * Creates a matcher node.
   * @param className The name of the matcher
   * @param values Pairs decribing value name/contents
   * @param variables Tuples describing variable name/sheet/column
   * @return An XML matcher node
   */
  def toMatcher( className: String,
		 values: Seq[ (String, String) ],
		 variables: Seq[ Tuple3[ String, String, Int ] ] ) = {
    <Matcher>
      { instanceInside( className, values, variables ) }
    </Matcher>
  }

  /**
   * Creates a replacer node
   * @param className The name of the replacer
   * @param values Pairs decribing value name/contents
   * @param variables Tuples describing variable name/sheet/column
   * @return An XML replacer node
   */
  def toReplacer( className: String,
		  values: Seq[ (String, String) ],
		  variables: Seq[ Tuple3[ String, String, Int ] ] ) = {
    <Replacer>
      { instanceInside( className, values, variables ) }
    </Replacer>
  }
}

import ParserCommon._

/**
 * The base class for parsers for ErrorChecker.pl's matchers and
 * replacers. There is a significant amount of commanality between the two
 * parsing routines.
 * @author Kyle Dewey
 */
trait Parser {
  /**
   * Gets what columns must have data
   * @return The columns that must have data
   */
  def okColumns() = Set( 0, 2 )

  /**
   * Gets what columns must not have data
   * @return the columns that must not have data
   */
  def notOkColumns() = Set( 1 )

  /**
   * Gets the column that has the module name
   * @return The column that has the module name
   */
  def moduleColumn() = 0

  /**
   * Gets a map holding dispatchers for module names.
   * @return A map holding dispatchers for module names
   */
  def dispatcher(): Map[ String, ( Seq[ String ], String, Int ) => Node ]

  /**
   * Dispatches based on module type.
   * @param moduleName The name of the module
   * @return Something that can parse module data, or None if there
   * isn't such a module recognized.
   */
  def dispatch( moduleName: String ) =
    dispatcher.get( moduleName )

  /**
   * Gets the modules that are understood by this
   * @return The modules that are understood for this
   */
  def validModules() =
    dispatcher.keySet

  /**
   * Determines if the given datum is valid
   * @param split The columns for this datum
   * @return true if it's valid, else false
   */
  def valid( split: Seq[ String ] ) =
    allDefined( stripOut( split, okColumns ) ) &&
    allUndefined( stripOut( split, notOkColumns ) ) &&
    dispatcher.keySet.contains( split( moduleColumn ) )
  
  /**
   * Converts the given datum to XML.
   * @param split The datums
   * @param sheet The spreadsheet name this is in reference to
   * @param column Which column this is in reference to
   * @return An XML definition of the same thing, or None if the split
   * isn't valid.
   */
  def toXML( split: Seq[ String ], sheet: String, column: Int ) = 
    if ( valid( split ) ) {
      Some( dispatch( split( moduleColumn ) ).get( split, sheet, column ) )
    } else {
      None
    }
}

/**
 * Holds rules for parsing in ErrorChecker.pl's matchers.
 * @author Kyle Dewey
 */
object MatcherParser extends Parser {
  val dispatcher = 
    Map( "exact" -> ( ( split: Seq[ String ], 
		        sheet: String, 
		        column: Int ) => exactToXML( split, sheet, column ) ),
	 "regex" ->  ( ( split: Seq[ String ],
		         sheet: String,
		         column: Int ) => regexToXML( split, sheet, column ) ),
	 "!exact" -> ( ( split: Seq[ String ],
		         sheet: String,
		         column: Int ) => notExactToXML( split, sheet, column ) ),
	 "!regex" -> ( ( split: Seq[ String ],
		         sheet: String,
		         column: Int ) => notRegexToXML( split, sheet, column ) ) )

  /**
   * Creates a matcher with a single value and variable.
   * @param moduleName The name of the matcher
   * @param valueName The name of the value
   * @param varName The name of the variable
   * @param split Holds data for the value
   * @param sheet The name of the sheet we are working with for the variable
   * @param column The column of the sheet we are working with for the variable
   * @return An XML matcher node encapsulating all of the given data
   */
  def oneValVarToXML( moduleName: String,
		      valueName: String,
		      varName: String,
		      split: Seq[ String ],
		      sheet: String,
		      column: Int ) = {
    toMatcher( moduleName,
	       Seq( (valueName, stripQuotes( split( 2 ) )) ),
	       Seq( Tuple3( varName, sheet, column ) ) )
  }

  /**
   * Creates a matcher that is equivalent to ErrorChecker.pl's
   * "exact" module, respective of negation on the ErrorChecker.pl module.
   * @param moduleName The name of the Error Sentinel module.  This is needed
   * since the same ErrorChecker.pl module exact and !exact are mapped to
   * two ErrorSentinel modules, String= and String!=, respectively.
   * @param split Holds data for the value
   * @param sheet The name of the sheet we are working with for the variable
   * @param column The column of the sheet we are working with for the variable
   * @return An XML matcher node encapsulating all of the given data
   */
  def exactModuleToXML( moduleName: String, 
		        split: Seq[ String ], 
		        sheet: String,
		        column: Int ) = {
    oneValVarToXML( moduleName,
		    "strings",
		    "strings",
		    split,
		    sheet, 
		    column )
  }
  
  /**
   * Creates a matcher that is compatible with ErrorChecker.pl's "exact"
   * matcher, without negation.
   * @param split Holds data for the value
   * @param sheet The name of the sheet we are working with for the variable
   * @param column The column of the sheet we are working with for the variable
   * @return An XML matcher node encapsulating all of the given data
   */
  def exactToXML( split: Seq[ String ], sheet: String, column: Int ) = 
    exactModuleToXML( "String=",
		      split,
		      sheet,
		      column )

  /**
   * Creates a matcher that is compatible with ErrorChecker.pl's "exact"
   * matcher, with negation.
   * @param split Holds data for the value
   * @param sheet The name of the sheet we are working with for the variable
   * @param column The column of the sheet we are working with for the variable
   * @return An XML matcher node encapsulating all of the given data
   */
  def notExactToXML( split: Seq[ String ], sheet: String, column: Int ) =
    exactModuleToXML( "String!=",
		      split,
		      sheet,
		      column )

  /**
   * Creates a matcher that is compatible with ErrorChecker.pl's "regex"
   * matcher.  The name of the Error Sentinel matcher must be passed along
   * since ErrorChecker.pl's regex maps to either Regex or NotRegex
   * depending on negation.
   * @param moduleName The name of the Error Sentinel module
   * @param split Holds data for the value
   * @param sheet The name of the sheet we are working with for the variable
   * @param column The column of the sheet we are working with for the variable
   * @return An XML matcher node encapsulating all of the given data
   */
  def regexModuleToXML( moduleName: String,
		        split: Seq[ String ],
		        sheet: String,
		        column: Int ) = {
    oneValVarToXML( moduleName,
		    "regex",
		    "string",
		    split,
		    sheet,
		    column )
  }

  /**
   * Creates a matcher that is compatible with ErrorChecker.pl's "regex"
   * matcher, without negation.
   * @param split Holds data for the value
   * @param sheet The name of the sheet we are working with for the variable
   * @param column The column of the sheet we are working with for the variable
   * @return An XML matcher node encapsulating all of the given data
   */
  def regexToXML( split: Seq[ String ], sheet: String, column: Int ) =
    regexModuleToXML( "Regex",
		      split,
		      sheet,
		      column )

  /**
   * Creates a matcher that is compatible with ErrorChecker.pl's "regex"
   * matcher, with negation.
   * @param split Holds data for the value
   * @param sheet The name of the sheet we are working with for the variable
   * @param column The column of the sheet we are working with for the variable
   * @return An XML matcher node encapsulating all of the given data
   */
  def notRegexToXML( split: Seq[ String ], sheet: String, column: Int ) =
    regexModuleToXML( "NotRegex",
		      split,
		      sheet,
		      column )
}

/**
 * Holds rules for parsing in ErrorChecker.pl's replacers.
 * @author Kyle Dewey
 */
object ReplacerParser extends Parser {
  val dispatcher = 
    Map( "exact" -> ( ( split: Seq[ String ],
		        sheet: String,
		        column: Int ) => exactToXML( split, sheet, column ) ) )

  def exactToXML( split: Seq[ String ], sheet: String, column: Int ): Node =
    exactToXML( split )

  def exactToXML( split: Seq[ String ] ): Node =
    toReplacer( "Concat",
	        Seq( ("data", stripQuotes( split( 2 ) )) ),
	        Seq() )
}

/*
 * Converts ErrorChecker.pl rules to Error Sentinel rules.
 * Note that it doesn't support all the modules that ErrorChecker.pl
 * supports, namely the database modules and any replacer that isn't
 * exact.  Given that the vast majority of existing ErrorChecker.pl rules do
 * not fall into this category, and that it is actually intended that
 * ErrorChecker.pl be abandoned upon completion of Error Sentinel, this is
 * not expected to be a problem.
 * @author Kyle Dewey
 */
object RuleConverter {
  import java.io._
  import java.util.Scanner

  /**
   * Converts the given split data to a good data XML node.
   * @param split The split data from the original line.
   * @param sheet The name of the spreadsheet variables are in respect to
   * @param column The column in the spreadsheet variables are in respect to
   * @return An XML good data node, or None if the data could not be parsed
   * to a GoodData XML node.
   */
  def toGoodData( split: Seq[ String ], sheet: String, column: Int ) = {
    val matcher = MatcherParser.toXML( split.take( 3 ), sheet, column )
    if ( matcher.isDefined ) {
      val xml =
	<GoodData>
	  { toCellRange( sheet, column ) }
	  { matcher.get }
	</GoodData>
      Some( xml )
    } else None
  }

  /**
   * Converts the given split data to an error correction XML node.
   * @param split The split data from the original line
   * @param sheet The name of the spreadsheet variables are in respect to
   * @param column The column in the spreadsheet variables are in respect to
   * @return An XML good data node, or None if the data could not be parsed
   * to an ErrorCorrection XML node.
   */
  def toErrorCorrection( split: Seq[ String ], sheet: String, column: Int ) = {
    val matcher = MatcherParser.toXML( split.take( 3 ), sheet, column )
    lazy val replacer = ReplacerParser.toXML( split.drop( 3 ), sheet, column )
    if ( matcher.isDefined &&
	 replacer.isDefined ) {
      val xml =
	<ErrorCorrection>
	  { matcher.get }
	  { replacer.get }
	  { toCellRange( sheet, column ) }
	</ErrorCorrection>
      Some( xml )
    } else None
  }

  /**
   * Prints usage information
   */
  def usage() {
    println( "Takes the following params: \n" +
	     "-ErrorChecker.pl file\n" +
	     "-Which sheet the file is for\n" +
	     "-Which column the file is for\n" +
	     "-Output XML filename" )
  }

  /**
   * Writes out the given information to the given file.
   * Note that there is some duplication with Writer again.
   * @param outputPath The path of the output file
   * @param goodData The good data XML nodes to write to the file
   * @param errorCorrection the error correction XML nodes to write to the file
   * @throws IOException If an error occurred on write
   */
  def writeFile( outputPath: String, 
		 goodData: Seq[ Node ], 
		 errorCorrection: Seq[ Node ] ) {
    val toWrite =
      <Associations>
	<GoodDatas>
	  { goodData }
	</GoodDatas>
	<ErrorCorrections>
	  { errorCorrection }
	</ErrorCorrections>
      </Associations>
    val output = new BufferedWriter( new FileWriter( outputPath ) )
    output.write( new PrettyPrinter( Integer.MAX_VALUE, 2 ).format( toWrite ) )
    output.close()
  }

  /**
   * Splits the given line on a tab.
   * @param line The line to parse
   * @param length The expected length of the split
   * @return The split, or None if it wasn't of the expected length
   */
  def splitLine( line: String, length: Int ) = {
    val split = line.split( "\t" )
    if ( split.length != length ) {
      println( "Line: " + line + "\n\t...has a split length of " +
	       split.length + ". Expected: " + length )
      None
    } else {
      Some( split )
    }
  }

  /**
   * Determines if the given line is a header line.
   * @param line The line to check
   * @return true if it's a header line, else false
   */
  def isHeaderLine( line: String ) =
    line.startsWith( "Match" )

  /**
   * Skips the header line.
   * @param input The input scanner
   */
  def skipHeader( input: Scanner ) {
    while( input.hasNextLine &&
	   !isHeaderLine( input.nextLine ) ) {}
  }

  /**
   * Parses the given line.
   * @param line The line to parse
   * @param sheet The name of the spreadsheet variables are in respect to
   * @param column The column that variables are in respect to
   * @param splitLength How long the line should be when split.  If the
   * actual split length doesn't match the expected split length, this
   * will return None without calling toNode
   * @param toNode Function that can convert the split line to a single XML
   * node.  If the line cannot be converted, it returns None.
   * @return An XML node encapsulating the information in the line, or None
   * if the line could not be converted to an XML node.
   */
  def parseLine( line: String, 
		 sheet: String, 
		 column: Int,
		 splitLength: Int,
		 toNode: ( Seq[ String ], String, Int ) => Option[ Node ] ) = {
    val split = splitLine( line, splitLength )
    if ( split.isDefined ) {
      toNode( split.get, sheet, column )
    } else {
      None
    }
  }

  /**
   * Parses in a good data line.
   * In other words, one of ErrorChecker.pl's good data matchers.
   * @param line The line to parse in
   * @param sheet The name of the spreadsheet variables are in respect to
   * @param column The column spreadsheet variables are in respect to
   * @return A GoodData XML node encapsulating the line information, or None
   * if the line couldn't be parsed into a good data node.
   */
  def parseGoodDataLine( line: String, sheet: String, column: Int ) = 
    parseLine( line, sheet, column, 3, toGoodData( _, _, _ ) )

  /**
   * Parses lines from the given scanner until either the scanner
   * runs out of lines or we meet some termination condition, whichever
   * comes first.
   * @param input The input scanner holding lines to read in
   * @param converter Converts the given line to some item.  The function
   * returns None if it cannot convert the given line for whatever reason.
   * @param until A user-defined termination condition
   * @return All items that were created that were non-None.
   * @throws IOException If a line could not be read from the scanner
   * for any reason
   */
  def parseLinesUntil[ T ]( input: Scanner, 
			    converter: String => Option[ T ],
			    until: String => Boolean ) = {
    var retval: List[ T ] = List()
    var run = true

    while( run && input.hasNextLine ) {
      val line = input.nextLine
      if ( until( line ) ) {
	run = false
      } else {
	val value = converter( line )
	if ( value.isDefined ) {
	  retval ::= value.get
	}
      }
    }

    retval.reverse
  }

  /**
   * Parses in the good data matchers section of an ErrorChecker.pl ruleset
   * file.
   * @param input Input scanner to read lines from
   * @param sheet The name of the spreadsheet that variables are made with
   * respect to
   * @param column The column that spreadshete variables are in respect to
   * @return Good data matchers that were recognized and successfully
   * parsed in.
   * @throws IOException If an error occurred on read
   */
  def parseGoodDataSection( input: Scanner, sheet: String, column: Int ) = 
    parseLinesUntil( input,
		     parseGoodDataLine( _, sheet, column ),
		     isHeaderLine( _ ) )

  /**
   * Parses in a line that is assumed to hold error correction information.
   * @param line The line to parse in
   * @param sheet The name of the spreadsheet that variables are created
   * in respect to
   * @param column The column that spreadsheet variables are created in
   * respect to
   * @return Either an XML error correction rule, or None if the line could
   * not be parsed as an error correction rule
   */
  def parseErrorCorrectionLine( line: String, sheet: String, column: Int ) = 
    parseLine( line, sheet, column, 6, toErrorCorrection( _, _, _ ) )

  /**
   * Parses in the error correction rule section of an ErrorChecker.pl file.
   * @param input The input scanner to read lines from
   * @param sheet The name of the spreadsheet that variables are made
   * in respect to
   * @param column The column that spreadsheet variables are made
   * in respect to
   * @return Error correction rules that were successfully parsed in
   * @throws IOException If an error occurs on read
   */
  def parseErrorCorrectionSection( input: Scanner, sheet: String, column: Int ) =
    parseLinesUntil( input,
		     parseErrorCorrectionLine( _, sheet, column ),
		     ( line: String ) => false ) // parse until end

  /**
   * Processes the given input ErrorChecker.pl ruleset file.
   * @param inputPath The path to the ErrorChecker.pl ruleset file
   * @param outputPath The path to the output XML file
   * @param sheet The name of the spreadsheet that variables are made
   * in respect to
   * @param column The column that spreadsheet variables are made
   * in respect to
   * @throws FileNotFoundException If the given input file could not be
   * found
   * @throws IOException If an error occurred on read
   */
  def processFile( inputPath: String, 
		   outputPath: String, 
		   sheet: String, 
		   column: Int ) {
    val input = new Scanner( new File( inputPath ) )
    skipHeader( input )

    val goodData = parseGoodDataSection( input, sheet, column )
    val errorCorrection = parseErrorCorrectionSection( input, sheet, column )

    writeFile( outputPath, 
	       goodData,
	       errorCorrection )
  } // processFile

  /**
   * The main function.  Runs the conversion program.
   * @param args Command line arguments (input path, spreadsheet name,
   * spreadsheet column, and output path)
   */
  def main( args: Array[ String ] ) {
    if ( args.length != 4 ) {
      usage()
    } else {
      processFile( args( 0 ), 
		   args( 3 ),
		   args( 1 ),
		   Integer.parseInt( args( 2 ) ) )
    }
  }
}

		   
