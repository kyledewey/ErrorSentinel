/*
 * ProjectHelpers.scala
 */

package sentinel.project

import sentinel.model._

/**
 * Container for a sheet.
 * @param name The name of the sheet
 * @param fileName The file associated with the sheet
 * @param fileType The type of the sheet
 * @author Kyle Dewey
 */
class Sheet( val name: String,
	     val fileName: String,
	     val fileType: String ) {}

/**
 * Container for a language definition
 * @param fileName The file associated with the language definition
 * @param fileType The type of the file
 * @author Kyle Dewey
 */
class Language( val fileName: String,
	        val fileType: String ) {
  lazy val classesInformation = loadClassesInformation

  /**
   * Loads in classes information for this language.
   * @return Pre class information for the given language
   * @throws UnknownLanguageTypeException If the given file type isn't known
   * @throws ClassParseException If an error occurred while parsing
   * @throws IOException If an error occurred on reading
   */
  def loadClassesInformation() =
    LanguageReader.classesInformation( this )
}

/**
 * Exception thrown when the sheet type is unknown
 * @param message A helpful message to show the user
 * @author Kyle Dewey
 */
case class UnknownSheetTypeException( message: String ) 
     extends Exception( message ) {}

/**
 * Definition for something that performs different actions based
 * upon an internal map.
 * @param map A mapping of strings to items that can perform actions
 * @author Kyle Dewey
 */
class ActionMap[ T ]( val map: Map[ String, T ] ) {
  /**
   * Gets all the recognized actions
   * @return A listing of the available actions, in abc order
   */
  def actions(): Seq[ String ] = 
    map.keys.toList.sortWith( _ < _ ).map( _.self )
}

import java.io.File
import javax.swing.filechooser.FileFilter

/**
 * A value for a file action map.
 * @param filter A filter that can accept files of the given type
 * @param typeName The type of the file
 * @param value The actual value
 * @author Kyle Dewey
 */
class FileActionMapValue[ T ]( val filter: FileFilter,
			       val typeName: String,
			       val value: T ) extends FileFilter {
  /**
   * Delegates to <code>filter.accept( file )</code>
   * @param file The file to text for acceptance
   * @return <code>filter.accept( file )</code>
   */
  def accept( file: File ) =
    filter.accept( file )

  /**
   * Delegates to <code>filter.getDescription</code>
   * @return <code>filter.getDescription</code>
   */
  def getDescription() =
    filter.getDescription
  
  /**
   * Converts this to a sheet, using the given name and file name.
   * @param name The name of the sheet
   * @param fileName The name of the file holding the sheet
   * @return A sheet object holding the above information
   */
  def toSheet( name: String, fileName: String ) =
    new Sheet( name, fileName, typeName )
}

import sentinel.io.input._
import sentinel.io.input.csv._

/**
 * A file action map value for csv files.
 * @param value The value for the map
 * @author Kyle Dewey
 */
class CSVValue[ T ]( value: T ) 
extends FileActionMapValue[ T ]( CSVFilter, "CSV", value ) {}

/**
 * A file action map value for tab delimited files.
 * @param value The value for the map
 * @author Kyle Dewey
 */
class TabValue[ T ]( value: T )
extends FileActionMapValue[ T ]( TabDelimitedFilter, "TAB", value ) {}

/**
 * Definition for an action map, where the actions are for
 * reading/writing files of different types.
 * @param map A mapping of strings to items that can read/write files
 * @author Kyle Dewey
 */
class FileActionMap[ T ]( map: Map[ String, FileActionMapValue[ T ] ] ) 
extends ActionMap[ FileActionMapValue[ T ] ]( map ) {
  /**
   * Uses the type names as keys for the map.
   * @param mapValues The values for the map
   */
  def this( mapValues: Seq[ FileActionMapValue[ T ] ] ) =
    this( Map() ++ mapValues.map( value => 
      (value.typeName, value) ) )

  /**
   * Gets all the recognized file types
   * @return A listing of all the available file types, in abc order
   */
  def fileTypes() =
    actions

  /**
   * Gets all the file filters in ABC order.
   * @return All the file filters in ABC order.
   */
  def filters() =
    actions.map( map( _ ) )
}

/**
 * Used for reading in sheets of different formats.
 * @author Kyle Dewey
 */
object SheetReader 
extends FileActionMap[ SpreadsheetReader ]( Seq( new CSVValue[ SpreadsheetReader ]( new ParseCSV() ),
						 new TabValue[ SpreadsheetReader ]( new ParseTabDelimited() ) ) ) {
  /**
   * Like <code>readSpreadsheet</code>, but it works with a sheet object.
   * @param sheet The sheet object
   * @param columnNames If the first row is the name of the columns
   * @return The spreadsheet read in
   * @throws UnknownSheetTypeException if the sheet type is unknown
   * @throws SpreadsheetReadException If there was an error in the underlying
   * format
   * @throws FileNotFoundException If the file could not be found
   * @throws IOException If some other reading error occurred
   * @throws SpreadsheetNameException If the name of a spreadsheet is invalid
   */
  def readSpreadsheet( sheet: Sheet, columnNames: Boolean ): Spreadsheet = {
    readSpreadsheet( sheet.name,
		     columnNames,
		     sheet.fileName,
		     sheet.fileType )
  }

  /**
   * Parses in a sheet of the given type.
   * @param name The name of the sheet to parse in
   * @param columnNames If the first row is the name of the columns
   * @param fileName The name of the file to parse in
   * @param fileType The file type of the sheet
   * @return The spreadsheet read in
   * @throws UnknownSheetTypeException if the sheet type is unknown
   * @throws SpreadsheetReadException If there was an error in the underlying
   * format
   * @throws FileNotFoundException If the file could not be found
   * @throws IOException If some other reading error occurred
   * @throws SpreadsheetNameException If the name of a spreadsheet is invalid
   */
  def readSpreadsheet( name: String,
		       columnNames: Boolean,
		       fileName: String,
		       fileType: String ): Spreadsheet = {
    if ( map.contains( fileType ) ) {
      map( fileType ).value.readSpreadsheet( name,
					     fileName,
					     columnNames )
    } else {
      throw new UnknownSheetTypeException( "Unknown input file type: " + 
					   fileType )
    }
  }
}

import sentinel.io.output._
import sentinel.io.output.csv._

/**
 * Used for writing out sheets in different formats.
 * @author Kyle Dewey
 */
object SheetWriter 
extends FileActionMap[ SpreadsheetWriter ]( Seq( new CSVValue[ SpreadsheetWriter ]( new WriteCSV() ),
						 new TabValue[ SpreadsheetWriter ]( new WriteTabDelimited() ) ) ) {
  import sentinel.model._

  /**
   * Like <code>writeSpreadsheet</code>, but it works with a Sheet object.
   * @param sheet The spreadsheet to write out
   * @param describer Describes the spreadsheet
   * @throws UnknownSheetTypeException If we can't understand the kind of
   * output format
   * @throws SpreadsheetWriteException If a format-level exception occurred
   * upon writing out the spreadsheet
   * @throws IOException If an error occurred on write
   */
  def writeSpreadsheet( sheet: Spreadsheet,
		        describer: Sheet ) {
    writeSpreadsheet( sheet,
		      describer.fileName,
		      describer.fileType )
  }

  /**
   * Writes out a spreadsheet
   * @param sheet The sheet to write out
   * @param fileName The name of the file to write it to
   * @param fileType The type of the file to write out as
   * @throws UnknownSheetTypeException If we can't understand the kind of
   * output format
   * @throws SpreadsheetWriteException If a format-level exception occurred
   * upon writing out the spreadsheet
   * @throws IOException If an error occurred on write
   */
  def writeSpreadsheet( sheet: Spreadsheet,
		        fileName: String,
		        fileType: String ) {
    if ( map.contains( fileType ) ) {
      map( fileType ).value.writeSpreadsheet( sheet,
				              fileName )
    } else {
      throw new UnknownSheetTypeException( "Unknown output file type: " +
					   fileType )
    }
  }
}

/**
 * File filter that recognizes xml
 * @author Kyle Dewey
 */
object XMLFilter extends FileFilter {
  /**
   * Merely accepts files with a .xml extension.
   * @param file The file to check
   * @return true if it ends with an XML extension, else false
   */
  def accept( file: File ) =
    file.getName.toLowerCase.endsWith( ".xml" )
  
  /**
   * Returns an xml file description.
   * @return A description of XML files
   */
  def getDescription() =
    "XML files"
}

/**
 * A file action map value for XML files.
 * @param value The value for the map
 * @author Kyle Dewey
 */
class XMLValue[ T ]( value: T )
extends FileActionMapValue[ T ]( XMLFilter, "XML", value ) {}

/**
 * Exception thrown when the given kind of language file isn't understood.
 * @param message An informative message for the user
 * @author Kyle Dewey
 */
case class UnknownLanguageTypeException( message: String ) 
     extends Exception( message ) {}

import sentinel.model.parser._
import sentinel.model.parser.xml._

/**
 * Used for reading in the error correction language in different formats.
 * @author Kyle Dewey
 */
object LanguageReader 
extends FileActionMap[ String => ClassParser ]( Seq( new XMLValue[ String => ClassParser ]( ( fileName: String ) => new XMLParser( fileName ) ) ) ) {
  // pair of language file/language file type
  val BASE_LANGUAGE = 
    Seq( new Language( "/Users/kyledewey/Documents/thesis/ErrorSentinel/code/sentinel/xml/builtins/matchers/base.xml", "XML" ),
	 new Language( "/Users/kyledewey/Documents/thesis/ErrorSentinel/code/sentinel/xml/builtins/matchers/arithmetic.xml", "XML" ),
	 new Language( "/Users/kyledewey/Documents/thesis/ErrorSentinel/code/sentinel/xml/builtins/matchers/character.xml", "XML" ),
	 new Language( "/Users/kyledewey/Documents/thesis/ErrorSentinel/code/sentinel/xml/builtins/matchers/string.xml", "XML" ),
	 new Language( "/Users/kyledewey/Documents/thesis/ErrorSentinel/code/sentinel/xml/builtins/matchers/database.xml", "XML" ),
	 new Language( "/Users/kyledewey/Documents/thesis/ErrorSentinel/code/sentinel/xml/builtins/replacers/base.xml", "XML" ),
	 new Language( "/Users/kyledewey/Documents/thesis/ErrorSentinel/code/sentinel/xml/builtins/replacers/arithmetic.xml", "XML" ),
	 new Language( "/Users/kyledewey/Documents/thesis/ErrorSentinel/code/sentinel/xml/builtins/replacers/character.xml", "XML" ),
	 new Language( "/Users/kyledewey/Documents/thesis/ErrorSentinel/code/sentinel/xml/builtins/replacers/string.xml", "XML" ),
	 new Language( "/Users/kyledewey/Documents/thesis/ErrorSentinel/code/sentinel/xml/builtins/replacers/database.xml", "XML" ) )

  /**
   * Reads in language files.
   * Note that dependencies can go across files, which is why it is
   * best to specify all languages as once.
   * @param languages Language defintions
   * @throws ClassParseException if an error occurred while parsing
   * @throws IOException if an error occurred on reading
   */
  def readLanguages( languages: Seq[ Language ] ) {
    if ( !languages.isEmpty ) {
      val preClassSet =
	Set() ++ languages.flatMap( _.classesInformation )
      ClassParser.toParseOrder( preClassSet )
                 .foreach( _.parseAndRegisterClass() )
    }
  }

  /**
   * Asserts that the given language type is known
   * @param theType The type of the language
   * @throws UnknownLanguageTypeException If the given file type isn't known
   */
  def assertLanguageType( theType: String ) {
    if ( !map.contains( theType ) ) {
      throw new UnknownLanguageTypeException( "Unknown language input file " +
					      "type: " + theType )
    }
  }

  /**
   * Gets pre class information for the given language
   * @param languge The language
   * @return Pre class information for the given language
   * @throws UnknownLanguageTypeException If the given file type isn't known
   * @throws ClassParseException If an error occurred while parsing
   * @throws IOException If an error occurred on reading
   */
  def classesInformation( language: Language ) = {
    assertLanguageType( language.fileType )
    map( language.fileType ).value( language.fileName ).classesInformation
  }

  /**
   * Reads in the base language
   * @throws ClassParseException if an error occurred while parsing
   * @throws IOException if an error occurred on reading
   */
  def readBaseLanguage() {
    readLanguages( BASE_LANGUAGE )
  }
}

import sentinel.model.writer._
import sentinel.model.writer.xml._

/**
 * Used for writing out the error correction language in different formats.
 * @author Kyle Dewey
 */
object LanguageWriter 
extends FileActionMap[ String => ClassWriter ]( Seq( new XMLValue[ String => ClassWriter ]( ( fileName: String ) => new XMLWriter( fileName ) ) ) ) {
  import sentinel.model._

  /**
   * Writes out the given classes to a file.
   * @param classes The classes to write out
   * @param fileName The name of the file to write to
   * @param fileType The type of the output file
   * @throws UnknownLanguageTypeException If the output file type was not
   * recognized
   * @throws ClassWriteException If a format-level error occurred on write
   * @throws IOException If an error occurred on writing
   */
  def writeLanguage( classes: Seq[ InstanceFactory[ _ ] ],
		     fileName: String,
		     fileType: String ) {
    if ( map.contains( fileType ) ) {
      map( fileType ).value( fileName ).writeClasses( classes )
    } else {
      throw new UnknownLanguageTypeException( "Unknown language output file " +
					      "type: " + fileType )
    }
  }
}

