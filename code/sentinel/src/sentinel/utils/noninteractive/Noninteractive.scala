/*
 * Noninteractive.scala
 *
 * Version:
 *     $Id: Noninteractive.scala,v 1.7 2011/06/01 04:04:16 kyledewey Exp $
 *
 * Revistions:
 *       $Log: Noninteractive.scala,v $
 *       Revision 1.7  2011/06/01 04:04:16  kyledewey
 *       Set to register spreadsheets.
 *
 *       Revision 1.6  2011/05/29 15:20:27  kyledewey
 *       Added the project being created to the spreadsheetFactory.
 *
 *       Revision 1.5  2011/05/28 19:57:58  kyledewey
 *       Now uses foreachRowColumn and foreachSpreadsheet.
 *
 *       Revision 1.4  2011/05/27 01:36:04  kyledewey
 *       Now uses a parameter for converting vanilla
 *       Spreadsheets to ReplacerSpreadsheets.
 *
 *       Revision 1.3  2011/05/25 20:19:44  kyledewey
 *       Major refactor.
 *       Uses the new model for error correction based on ErrorChecker.pl.
 *       Error corrections now performed at the cell level instead
 *       of at the row level.
 *
 *       Revision 1.2  2010/07/17 02:07:50  kyledewey
 *       Now compliant with the new spreadsheet class hierarchy.
 *
 *       Revision 1.1  2010/07/11 05:57:48  kyledewey
 *       Initial revision
 *
 *
 */

package sentinel.utils.noninteractive

import sentinel.model._
import sentinel.project._

/**
 * Contains helper methods for ProjectRunner.
 * @author Kyle Dewey
 */
object ProjectRunner {
  /**
   * Creates a spreadsheet based upon another spreadsheet.
   * The new sheet will have the same number of columns and the same
   * column identifiers, but the data will be empty
   * @param sheet The sheet to base this one off of
   * @return A spreadsheet based on the other spreadsheet according
   * to the above description
   */
  def makeSpreadsheet( sheet: ReplacerSpreadsheet ) = {
    val retval = Spreadsheet( "", false )
    0.until( sheet.getColumnCount ).foreach( column => retval.addColumn() )
    0.until( sheet.getRowCount ).foreach( row => retval.addRow() )
    retval.setColumnIdentifiers( sheet.getColumnIdentifiers )
    retval
  }

  /**
   * Processes a cell of the spreadsheet.
   * @param cellValue The value of the cell here
   * @param instanceResult The result of calling <code>tryValueAt</code> for
   * this value at its proper position
   * @return A tuple holding the following:
   * <ul>
   * <li>What to put in the clean sheet here</li>
   * <li>What to put in the unclean sheet here</li>
   * <li>What to put in the why unclean sheet here</li>
   * </ul>.
   * Note for all of these None means the cell should be used as a filler,
   * but nothing more.
   */
  def processCell( cellValue: String, instanceResult: Option[ InstanceResult ] ) = {
    var retval: Tuple3[ Option[ String ], Option[ String ], Option[ String ] ] = 
      Tuple3( None, None, None )
    def success( s: InstanceSuccess ) {
      retval = ( Some( cellValue ), None, None )
    }
    def failureReplacement( r: InstanceFailureReplacement ) {
      retval = ( Some( r.replacement ), None, None )
    }
    def failureException( e: InstanceFailureException[ _ ] ) {
      val whyUnclean = 
	if ( e.exception.getMessage != null ) {
	  Some( e.exception.getMessage )
	} else {
	  Some( "Replacer threw exception without message" )
	}
      retval = ( None, Some( cellValue ), whyUnclean )
    }
    def totalFailure() {
      retval = ( None, Some( cellValue ), Some( "Failed good data but had no replacement rule" ) )
    }
    InstanceResult.dispatchOnInstanceResult( instanceResult,
					     success( _ ),
					     failureReplacement( _ ),
					     failureException( _ ),
					     totalFailure )
    retval
  }
}

/**
 * Loads in a project in its entirety, performs error correction, and
 * outputs the results to three spreadsheet files.  The first
 * output file consists of clean, corrected data.  The second consists of
 * malformed, uncorrectable data, and the third consists of a parallel
 * file of reasons for the errors.  That is, each row in the malformed data
 * file correlates to a rule in the third file of explanations.
 * @param projectFile The file associated with the project
 * @param projectType the type of the project file
 * @param cleanOutputFile The name of the file to write to for the output.
 * Note that this is intended as a prefix; the real output file will be
 * of the form cleanOutputFile.originalFileName
 * @param cleanOutputType The type of the clean output file type
 * @param badOutputFile The name of the file to write to for bad data.
 * This is also a prefix.
 * @param badOutputType The type of the bad output file
 * @param whyOutputFile The name of the file to write to for reasons for 
 * bad data.  This is also a prefix.
 * @param whyOutputType The type of that particular file
 * @throws UnknownSheetTypeException If the type of the sheet is unknown
 * @throws UnknownLanguageTypeException If the type of a language file
 * is unknown
 * @throws UnknownProjectTypeException If the type of the project file is
 * unknown
 * @throws ClassParseException If there was an error in a language file
 * @throws SpreadsheetWriteException If there was an error on writing
 * out a spreadsheet
 * @throws ParameterizedInstantiationException If we could not instantiate
 * a replacer
 * @throws SpreadsheetReadException If there was an error in the underlying
 * spreadsheet format
 * @throws FileNotFoundException If the file could not be found
 * @throws SpreadsheetNameException If the name of a spreadsheet is invalid
 * @throws IOException If some other reading error occurred
 * @author Kyle Dewey
 */
class ProjectRunner( val projectFile: String,
		     val projectType: String,
		     val cleanOutputFile: String,
		     val cleanOutputType: String,
		     val badOutputFile: String,
		     val badOutputType: String,
		     val whyOutputFile: String,
		     val whyOutputType: String ) {
  LanguageReader.readBaseLanguage()
  val project = 
    ProjectReader.readProject( projectFile,
			       projectType,
			       ( sheet, project: Project[ _ ], register ) =>
				 ReplacerSpreadsheet( sheet, register ),
			       true )
  /**
   * Processes the given spreadsheet.
   * @param sheet The spreadsheet to process
   */
  def processSpreadsheet( sheet: ReplacerSpreadsheet ) {
    import ProjectRunner._
    // make the output sheets
    val cleanSheet = makeSpreadsheet( sheet )
    val uncleanSheet = makeSpreadsheet( sheet )
    val whyUncleanSheet = makeSpreadsheet( sheet )

    // process each cell
    sheet.foreachRowColumn( ( row, column ) => {
      def setSheet( value: Option[ String ], sheet: Spreadsheet ) {
	if ( value.isDefined ) {
	  sheet.setValueAt( value.get, row, column )
	}
      }
      val currentValue = sheet.getValueAt( row, column ).toString
      val instanceResult = sheet.tryValueAt( currentValue, row, column )
      val ( clean,
	    unclean,
	    whyUnclean ) = processCell( currentValue,
				        instanceResult )
      setSheet( clean, cleanSheet )
      setSheet( unclean, uncleanSheet )
      setSheet( whyUnclean, whyUncleanSheet )
    } )
    
    // write the output
    def writeSpreadsheet( toWrite: Spreadsheet, 
			  filePrefix: String,
			  fileType: String ) {
      SheetWriter.writeSpreadsheet( toWrite,
			            project.sheets( sheet.name ).fileName +
				    "." + filePrefix,
			            fileType )
    }
    writeSpreadsheet( cleanSheet,
		      cleanOutputFile,
		      cleanOutputType )
    writeSpreadsheet( uncleanSheet,
		      badOutputFile,
		      badOutputType )
    writeSpreadsheet( whyUncleanSheet,
		      whyOutputFile,
		      whyOutputType )
  }

  /**
   * Executes the project.
   */
  def executeProject() {
    project.foreachSpreadsheet( processSpreadsheet( _ ) )
  }
} // ProjectRunner

/**
 * The noninteractive spreadsheet program.
 * @author Kyle Dewey
 */
object ErrorSentinel {
  /**
   * Prints usage information
   */
  def usage() {
    println( "Takes the following params:" )
    println( "-Path to project file" )
    println( "-Type of project file" )
    println( "-Prefix for clean output files" )
    println( "-Type for clean output files" )
    println( "-Prefix for bad output files" )
    println( "-Type for bad output files" )
    println( "-Prefix for why output files" )
    println( "-Type for why output files" )
  }

  def main( args: Array[ String ] ) {
    if ( args.length != 8 ) {
      usage()
      exit( 1 )
    }
    new ProjectRunner( args( 0 ),
		       args( 1 ),
		       args( 2 ),
		       args( 3 ),
		       args( 4 ),
		       args( 5 ),
		       args( 6 ),
		       args( 7 ) ).executeProject()
  }
}
