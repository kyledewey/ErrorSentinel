/*
 * Parser.scala
 */

package sentinel.project

import java.io._
import sentinel.model._

/**
 * Exception thrown when there was an error on read in a project's definition.
 * @param message An informative message to show the user
 * @author Kyle Dewey
 */
case class ProjectParseException( message: String )
     extends IOException( message ) {}

/**
 * Interface that describes something that can read in a project
 * @author Kyle Dewey
 */
trait ProjectParser {
  /**
   * Parses in a project in from the given file
   * @param file The file to read from
   * @param spreadsheetFactory Something that can convert spreadsheets
   * to a given desired format.  Takes a spreadsheet, the project being made,
   * and whether or not to register the spreadsheet as a parameter
   * @param register Whether or not to register the created spreadsheets
   * @throws ProjectParseException If a format-level reading error occurred
   * @throws FileNotFoundException If the file could not be opened
   * @throws IOException If an error occurred on read
   */
  def parseProject[ T <: Spreadsheet ]( file: File, 
				        spreadsheetFactory: ( Spreadsheet, Project[ T ], Boolean ) => T,
				        register: Boolean ): Project[ T ]

  /**
   * Parses in a project in from the given file
   * @param fileName The name of the file to read from
   * @param spreadsheetFactory Something that can convert spreadsheets
   * to a given desired format.  Takes a spreadsheet, the project being made,
   * and whether or not to register the spreadsheet as a parameter.
   * @param register Whether or not to register the created spreadsheets
   * @throws ProjectParseException If a format-level reading error occurred
   * @throws FileNotFoundException If the file could not be opened
   * @throws IOException If an error occurred on read
   */
  def parseProject[ T <: Spreadsheet ]( fileName: String, 
				        spreadsheetFactory: ( Spreadsheet, Project[ T ], Boolean ) => T,
				        register: Boolean ): Project[ T ] = 
    parseProject( new File( fileName ), 
		  spreadsheetFactory,
	          register )
}

/**
 * Something that can read in a project in XML format.
 * @author Kyle Dewey
 */
object ParseXML extends ProjectParser {
  import scala.xml._

  def getNodes( base: Node, tag: String ) =
    XMLHelpers.getNodes( base, tag, ProjectParseException( _ ) )

  def getText( base: Node, tag: String ) =
    XMLHelpers.getText( base, tag, ProjectParseException( _ ) )

  /**
   * Given the text of a row, it will return the corresponding row
   * @param text The text of the row
   * @return The row, or None if it's a reference to the current row
   * @throws ProjectParseException If an actual row was passed, but it
   * wasn't an integer
   */
  def textToRow( text: String ): Option[ Int ] = {
    import sentinel.model.parser._
    try {
      ClassParser.textToRow( text )
    } catch {
      case e: ClassParseException =>
	throw new ProjectParseException( e.getMessage )
    }
  }

  /**
   * Given the text of a column, it will return the corresponding
   * column.
   * @param text The text of the column
   * @return The column, or None if it's a reference to the current column
   * @throws ProjectParseException If an actual column was passed, but it
   * wasn't an integer
   */
  def textToColumn( text: String ): Option[ Int ] = {
    import sentinel.model.parser._
    try {
      ClassParser.textToColumn( text )
    } catch {
      case e: ClassParseException =>
	throw new ProjectParseException( e.getMessage )
    }
  }

  /**
   * Parses in a cell range.
   * @param range The node containing cell range information
   * @return A new cell range encapsulating the information in the cell
   * range
   * @throws ProjectParseException If required information is missing
   */
  def parseCellRangeNode( range: Node ) = {
    import sentinel.model.parser._
    new CellRange( ClassParser.textToSpreadsheet( getText( range, "Sheet" ) ),
		   textToRow( getText( range, "Row" ) ),
		   textToColumn( getText( range, "Column" ) ) )
  }

  /**
   * Parses in a value node
   * @param valueNode The value node
   * @return A named parameter holding the value information
   * @throws ProjectParseException If required information is missing or
   * exising information is invalid
   */
  def parseValueNode( valueNode: Node ) = {
    val ( name,
	  theType ) = getNameAndType( valueNode )
    val value = getText( valueNode, "Contents" )
    val asParam = Constant( theType, value )
    if ( asParam.isEmpty ) {
      throw new ProjectParseException( "Value \"" + value + "\" is " +
				       "incompatible with type " +
				       ParamType.paramToString( theType ) )
    } else {
      new NamedParam( name, asParam.get )
    }
  }
    
  /**
   * Parses in a values node
   * @param values The values node
   * @return Named parameters holding the values
   * @throws ProjectParseException If required information is missing or
   * exising information is invalid
   */
  def parseValuesNode( values: Node ) =
    ( values \ "Value" ).map( parseValueNode( _ ) )

  /**
   * Gets the name and type of a variable or value node
   * @param node The variable or value node
   * @return The name and the type in a pair
   * @throws ProjectParseException If required information is missing or
   * exising information is invalid
   */
  def getNameAndType( node: Node ) = {
    var typeName = ""

    try {
      typeName = getText( node, "Type" )
      (getText( node, "Name" ),
       ParamType.stringToParam( typeName ))
    } catch {
      case e: NoSuchElementException => 
	throw new ProjectParseException( "Unknown variable type: " + typeName )
    }
  }

  /**
   * Parses in a variable node
   * @param variable The variable node
   * @return A new named parameter holding the variable
   * @throws ProjectParseException If required information is missing or
   * exising information is invalid
   */
  def parseVariableNode( variable: Node ) = {
    import java.util.NoSuchElementException
    var theType = ParamType.StringType
    try {
      val pair = getNameAndType( variable )
      val name = pair._1
      theType = pair._2
      val range = parseCellRangeBelow( variable )
      new NamedParam( name, 
		      new SpreadsheetVariable( range ) )
    } catch {
      case e: UnknownSpreadsheetVariableType =>
	throw new ProjectParseException( "Unknown spreadsheet variable type: " +
					 ParamType.toString( theType ) )
    }
  }

  /**
   * Parses in a variables node
   * @param variables The variables node
   * @retrun a sequence of named parameters correlating to the variables
   * @throws ProjectParseException If required information is missing or
   * exising information is invalid
   */
  def parseVariablesNode( variables: Node ) =
    ( variables \ "Variable" ).map( parseVariableNode( _ ) )

  /**
   * Parses in an instance factory node.
   * @param instanceNode The node containing instance information
   * @return The instance associated with the given node
   * @throws ProjectParseException If required information is missing,
   * or there is no such factory with this name.  Also if we could not
   * instantiate it.
   * @pre Languages that are associated with this project parsed in in full
   */
  def parseInstanceNode( instanceNode: Node ) = {
    val className = getText( instanceNode, "ClassName" )
    try {
      val factory = FactoryManager.getFactory( instanceNode.label,
					       className )
      val variables =
	parseVariablesNode( getNodes( instanceNode, "Variables" ).head )
      val values =
	parseValuesNode( getNodes( instanceNode, "Values" ).head )
      
      factory.get.instantiate( variables ++ values, false )
    } catch {
      case e: NoSuchElementException =>
	throw new ProjectParseException( "No such factory with name: " + className )
      case e: UnknownFactoryTypeException => 
	throw new ProjectParseException( e.toString )
      case e: ParameterNameException =>
	throw new ProjectParseException( e.toString )
      case e: ParameterRequirementException =>
	throw new ProjectParseException( e.toString )
      case e: ParameterTypeException =>
	throw new ProjectParseException( e.toString )
      case e: ParameterArrayException =>
	throw new ProjectParseException( e.toString )
      case e: ParameterizedInstantiationException => 
	throw new ProjectParseException( e.toString )
    }
  } // parseInstanceNode
  
  /**
   * Parses in the first instance underneath the given node
   * @param node The base node
   * @param instanceType Either "Matcher" or "Replacer"
   * @throws ProjectParseException If required information is missing or
   * invalid
   */
  def parseInstanceBelow( node: Node, instanceType: String ) =
    parseInstanceNode( getNodes( node, instanceType ).head )

  /**
   * Parses in the first cell range node beneath the given node
   * @param node The base node
   * @throws ProjectParseException If required information is missing or
   * invalid
   */
  def parseCellRangeBelow( node: Node ) =
    parseCellRangeNode( getNodes( node, "CellRange" ).head )

  /**
   * Parses in a "GoodData" node.
   * @param goodData The good data node
   * @return A pair holding [ matcher, cellRange ]
   * @throws ProjectParseException If required information is missing or
   * invalid
   */
  def parseGoodDataNode( goodData: Node ) =
    (parseInstanceBelow( goodData, "Matcher" ).asInstanceOf[ Matcher ],
     parseCellRangeBelow( goodData ))
  
  /**
   * Parses in a "GoodDatas" node.
   * @param goodDatas The good datas node holding good data information
   * @return a sequence of [ goodData, range ] pairs
   * @throws ProjectParseException If required information is missing or
   * invalid
   */
  def parseGoodDatasNode( goodDatas: Node ) =
    ( goodDatas \ "GoodData" ).map( parseGoodDataNode( _ ) )

  /**
   * Parses in an "ErrorCorrection" node
   * @param errorCorrection the error correction node
   * @return A pair holding the matcher/replacer pair, along with
   * the cell range that it applies to
   * @throws ProjectParseException If required information is missing or
   * invalid
   */
  def parseErrorCorrectionNode( errorCorrection: Node ) = {
    val matcher = parseInstanceBelow( errorCorrection, "Matcher" )
    val replacer = parseInstanceBelow( errorCorrection, "Replacer" )
    val range = parseCellRangeBelow( errorCorrection )
    ((matcher.asInstanceOf[ Matcher ], 
      replacer.asInstanceOf[ Replacer ]),
     range)
  }
  
  /**
   * Parses in an "ErrorCorrections" node
   * @param errorCorrections The error corrections node
   * @return A seq holding all the error corrections
   * @throws ProjectParseException If required information is missing or
   * invalid
   */
  def parseErrorCorrectionsNode( errorCorrections: Node ) =
    ( errorCorrections \ "ErrorCorrection" ).map( parseErrorCorrectionNode( _ ) )

  /**
   * Parses in the associations node
   * @param associations The node containing associations information
   * @return A new associations encapsulating the information in the
   * association
   * @throws ProjectParseException If required information is missing
   * or invalid
   */
  def parseAssociationsNode( associations: Node ) = {
    val goodData =
      parseGoodDatasNode( getNodes( associations, "GoodDatas" ).head )
    val errorCorrections =
      parseErrorCorrectionsNode( getNodes( associations, "ErrorCorrections" ).head )
    new Associations( goodData, errorCorrections )
  }
  
  /**
   * Parses in a sheet node.
   * @param sheet The sheet node
   * @return A new sheet, encapsulating the information in the node
   * @throws ProjectParseException If some required information was missing
   */
  def parseSheetNode( sheet: Node ) =
    new Sheet( getText( sheet, "Name" ),
	       getText( sheet, "File" ),
	       getText( sheet, "Type" ) )
  
  /**
   * Parses in a sheets node
   * @param sheets The sheets to parse in
   * @return A new sequence of sheets, encapsulating the information
   * in the node
   * @throws ProjectParseException If some required information was missing
   */
  def parseSheetsNode( sheets: Node ): Seq[ Sheet ] =
    ( sheets \ "Sheet" ).map( parseSheetNode( _ ) )
  
  /**
   * Parses in a definition node.
   * @param definition The definition node to parse in
   * @return A new language definition encapsulating the information
   * @throws ProjectParseException If some required information was missing
   * @throws UnknownLanguageTypeException If the given file type isn't known
   * @throws ClassParseException If an error occurred while parsing
   * @throws IOException If an error occurred on reading
   */
  def parseDefinitionNode( definition: Node ) = {
    val file = getText( definition, "File" )
    val theType = getText( definition, "Type" )
    new Language( file, theType )
  }
  
  /**
   * Parses in a definitions node
   * @param definitions The definitions node to parse in
   * @return A new sequence of language definitions encapsulating the
   * information
   * @throws ProjectParseException If some required information was missing
   * @throws UnknownLanguageTypeException If the given file type isn't known
   * @throws ClassParseException If an error occurred while parsing
   * @throws IOException If an error occurred on reading
   */
  def parseDefinitionsNode( definitions: Node ): Seq[ Language ] =
    ( definitions \ "Definition" ).map( parseDefinitionNode( _ ) )
  
  /**
   * Parses in a project node.
   * @param project The project node
   * @param spreadsheetFactory Something that can convert spreadsheets
   * to a given desired format.  Takes a spreadsheet and whether or not to
   * register the spreadsheet as a parameter
   * @param register Whether or not to register the spreadsheets
   * @return A project
   * @throws ProjectParseException If some required information was missing
   * @throws UnknownLanguageTypeException If the given file type isn't known
   * @throws ClassParseException If an error occurred while parsing
   * @throws ParameterizedInstantiationException If we could not
   * instantiate a replacer
   * @throws IOException If an error occurred on reading
   */
  def parseProjectNode[ T <: Spreadsheet ]( project: Node, 
					    spreadsheetFactory: ( Spreadsheet, Project[ T ], Boolean ) => T,
					    register: Boolean ) = {
    val sheets = parseSheetsNode( getNodes( project, "Sheets" ).head )
    val definitions = 
      parseDefinitionsNode( getNodes( project, "Definitions" ).head )
    LanguageReader.readLanguages( definitions )
    val associations =
      parseAssociationsNode( getNodes( project, "Associations" ).head )
    new Project[ T ]( Map() ++ sheets.map( sheet => sheet.name -> sheet ),
		      definitions,
	              associations,
		      spreadsheetFactory,
		      register )
  }

  /**
   * Parses in a project in from the given file
   * @param file The file to read from
   * @param spreadsheetFactory Something that can convert spreadsheets
   * to a given desired format.  Takes a spreadsheet and whether or not to
   * register the spreadsheet as a parameter
   * @param register Whether or not to register the created spreadsheet
   * @throws ProjectParseException If a format-level reading error occurred
   * @throws FileNotFoundException If the file could not be opened
   * @throws UnknownLanguageTypeException If the given file type isn't known
   * @throws ClassParseException If an error occurred while parsing
   * @throws ParameterizedInstantiationException If we could not
   * instantiate a replacer
   * @throws IOException If an error occurred on reading
   */
  def parseProject[ T <: Spreadsheet ]( file: File, 
				        spreadsheetFactory: ( Spreadsheet, Project[ T ], Boolean ) => T,
				        register: Boolean ) = {
    val contents = XML.loadFile( file )
    parseProjectNode( contents, 
		      spreadsheetFactory,
		      register )
  }
}
