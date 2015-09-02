/*
 * Writer.scala
 */

package sentinel.project

import java.io._
import sentinel.model._

/**
 * Exception thrown when there was an error on writing out a project's
 * definition.
 * @param message An informative message to show the user
 * @author Kyle Dewey
 */
case class ProjectWriteException( message: String )
     extends IOException( message ) {}

/**
 * Interface that describes something that can write out a project
 * @author Kyle Dewey
 */
trait ProjectWriter {
  /**
   * Writes the given project out to the given file
   * @param project The file to write to
   * @param file The file to write to
   * @throws ProjectWriteException If a format-level writing error occurred
   * @throws IOException If an error occurred on write
   */
  def writeProject( project: Project[ _ ],
		    file: File ): Unit

  /**
   * Writes the given project out to the given file
   * @param project The file to write to
   * @param fileName The name of the file to write to
   * @throws ProjectWriteException If a format-level writing error occurred
   * @throws IOException If an error occurred on write
   */
  def writeProject( project: Project[ _ ],
		    fileName: String ) {
    writeProject( project,
		  new File( fileName ) )
  }
}

/**
 * Something that can write out a project in XML format.
 * @author Kyle Dewey
 */
object WriteXML extends ProjectWriter {
  import scala.xml._

  /**
   * Given a cell range, creates an XML representing the cell range
   * @param range The range to write out
   * @return An XML node describing the cell range
   */
  def cellRangeToNode( range: CellRange ) = {
    import Spreadsheet._
    <CellRange>
      <Sheet>{ range.sheet.getOrElse( ANY_SHEET ) }</Sheet>
      <Row>{ range.row.getOrElse( ANY_ROW ) }</Row>
      <Column>{ range.column.getOrElse( ANY_COLUMN ) }</Column>
    </CellRange>
  }

  /**
   * Given a named parameter, it will return the name and the type
   * in a single nodeseq
   * @param namedParam The named parameter
   */
  def namedParamNameType( namedParam: NamedParam ) = {
    <Name>{ namedParam.name }</Name>
    <Type>{ namedParam.param.typeName }</Type>
  }

  /**
   * Converts the given constant to a value node
   * @param namedParam The named parameter
   * @return An XML value node corresponding to the value
   * @throws ProjectWriteException If the underlying parameter isn't a constant
   */
  def constantParamToNode( namedParam: NamedParam ) = {
    if ( !namedParam.param.isConstantType ) {
      throw new ProjectWriteException( "Given parameter is not a constant: " +
				       namedParam.name )
    }

    <Value>
      { namedParamNameType( namedParam ) }
      <Contents>{ namedParam.param.sentStringValue }</Contents>
    </Value>
  }

  /**
   * Converts the given named parameter to a variable node
   * @param namedParam The named parameter
   * @return An XML variable node encapsulating all the information in it
   * @throws ProjectWriteException If an underlying parameter isn't a variable
   */
  def variableParamToNode( namedParam: NamedParam ) = {
    if ( !namedParam.param.isInstanceOf[ CellRange ] ) {
      throw new ProjectWriteException( "Given parameter is not a variable: " +
				       namedParam.name )
    }
    <Variable>
    { namedParamNameType( namedParam ) }
    { cellRangeToNode( namedParam.param.asInstanceOf[ CellRange ] ) }
    </Variable>
  }

  /**
   * Given named parameters, it will convert variables to a Variables
   * node.
   * @param namedParams The named parameters
   * @throws ProjectWriteException If a given parameter was expected to be
   * a variable but isn't
   */
  def namedParamsToVariablesNode( namedParams: Seq[ NamedParam ] ) = {
    <Variables>
    { namedParams.filter( _.param.isVariableType )
                           .map( variableParamToNode( _ ) ) }
    </Variables>
  }

  /**
   * Given named parameters, it will convert constants to a Values node.
   * @param namedParams The named parameters
   * @throws ProjectWriteException If a given parameter was expected to be
   * a constant but isn't
   */
  def namedParamsToValuesNode( namedParams: Seq[ NamedParam ] ) = {
    <Values>
    { namedParams.filter( _.param.isConstantType )
                 .map( constantParamToNode( _ ) ) }
    </Values>
  }

  /**
   * Converts the given named parameters to a Variables and a Values node
   * @param namedParams The named parameters
   * @return nodes for both variables and constants
   * @throws ProjectWriteException If an underlying parameter isn't a variable
   */
  def namedParamsToNodes( namedParams: Seq[ NamedParam ] ) = 
    namedParamsToValuesNode( namedParams ) ++ 
      namedParamsToVariablesNode( namedParams )

  /**
   * Converts the given class name to a node
   * @param className The class name
   * @return An XML node holding the class name
   */
  def classNameToNode( className: String ) =
    <ClassName>{ className }</ClassName>

  /**
   * Gets the inner part of a node for an instance.
   * This portion does not differ between matchers and replacers
   * @param instance The instance
   * @return Two XML nodes in a NodeSeq: one for the class name, and the
   * other for the variables
   * @throws ProjectWriteException If an underlying parameter isn't a variable
   */
  def instanceInnerNodes( instance: Instance ) = 
    classNameToNode( instance.className ) ++ 
      namedParamsToNodes( instance.params )

  /**
   * Converts the given instance to a node
   * @param instance The instance
   * @return An XML node holding all the instance information
   * @throws ProjectWriteException If an underlying parameter isn't a variable,
   * or if the instance is neither a matcher or a replacer
   */
  def instanceToNode( instance: Instance ) = {
    import ParamType._
    lazy val inner = instanceInnerNodes( instance )
    instance.getType match {
      case MatcherType => <Matcher>{ inner }</Matcher>
      case ReplacerType => <Replacer>{ inner }</Replacer>
      case _ => throw new ProjectWriteException( "Unknown instance type: " +
						 instance.typeName )
    }
  }

  /**
   * Given good data matcher information, it will create an XML node
   * encapsualting all of it.
   * @param goodData The good data information from an associations object
   * @return An XML node describing the good data
   * @throws ProjectWriteException If an underlying parameter isn't a variable,
   * or if the instance is neither a matcher or a replacer
   */
  def goodDataToNode( goodData: (Matcher, CellRange) ) = {
    <GoodData>
      { cellRangeToNode( goodData._2 ) }
      { instanceToNode( goodData._1 ) }
    </GoodData>
  }

  /**
   * Given all the good data matcher information, it will create an XML
   * node encapsulating all of it
   * @param goodDatas The good data information from an association
   * @return An XML node describing all the good data matchers
   * @throws ProjectWriteException If an underlying parameter isn't a variable,
   * or if the instance is neither a matcher or a replacer
   */
  def goodDatasToNode( goodDatas: Seq[ (Matcher, CellRange) ] ) = {
    <GoodDatas>
      { goodDatas.map( goodDataToNode( _ ) ) }
    </GoodDatas>
  }

  /**
   * Given an error correction pair, it will create a single node that
   * encapsulates all the information within.
   * @param errorCorrectionPair The matcher/replacer pair, along with the
   * cell range that it applies to
   * @return An XML node that holds all of the information
   * @throws ProjectWriteException If an underlying parameter isn't a variable,
   * or if the instance is neither a matcher or a replacer
   */
  def errorCorrectionPairToNode( errorCorrectionPair: ((Matcher, Replacer), CellRange) ) = {
    <ErrorCorrection>
      { instanceToNode( errorCorrectionPair._1._1 ) }
      { instanceToNode( errorCorrectionPair._1._2 ) }
      { cellRangeToNode( errorCorrectionPair._2 ) }
    </ErrorCorrection>
  }

  /**
   * Given error correction pairs, it will create a single node
   * that encapsulates the information
   * @param errorCorrectionPairs The matcher/replacer error correction pairs,
   * along with cell ranges that they apply to
   * @return An XML node that holds all the information within
   * @throws ProjectWriteException If an underlying parameter isn't a variable,
   * or if the instance is neither a matcher or a replacer
   */
  def errorCorrectionPairsToNode( errorCorrectionPairs: Seq[ ((Matcher, Replacer), CellRange) ] ) = {
    <ErrorCorrections>
      { errorCorrectionPairs.map( errorCorrectionPairToNode( _ ) ) }
    </ErrorCorrections>
  }

  /**
   * Creates an associations node, given an associations object
   * @param associations The associations to write out
   * @return An XML node describing the associations
   */
  def associationsToNode( associations: Associations ) = {
    <Associations>
      { goodDatasToNode( associations.goodDataMatchers ) }
      { errorCorrectionPairsToNode( associations.errorCorrectionPairs ) }
    </Associations>
  }

  /**
   * Creates a sheet node, given a sheet
   * @param sheet The sheet to write out
   * @return A node containing the sheet
   */
  def sheetToNode( sheet: Sheet ) = {
    <Sheet>
      <Name>{ sheet.name }</Name>
      <File>{ sheet.fileName }</File>
      <Type>{ sheet.fileType }</Type>
    </Sheet>
  }

  /**
   * Creates a sheets node, given a bunch of sheets
   * @param sheets The sheets to write out
   * @return A node containing all the sheets
   */
  def sheetsToNode( sheets: Seq[ Sheet ] ) = {
    <Sheets>
      { sheets.map( sheetToNode( _ ) ) }
    </Sheets>
  }

  /**
   * Creates a definition node, given a language definition file
   * @param definition The language definition
   * @return A node containing the definition information
   */
  def definitionToNode( definition: Language ) = {
    <Definition>
      <File>{ definition.fileName }</File>
      <Type>{ definition.fileType }</Type>
    </Definition>
  }

  /**
   * Creates a definitions node, given a bunch of language definitions
   * @param definitions The definitions to write out
   * @return A node containing all the definitions
   */
  def definitionsToNode( definitions: Seq[ Language ] ) = {
    <Definitions>
      { definitions.map( definitionToNode( _ ) ) }
    </Definitions>
  }

  /**
   * Creates a project node, given a project
   * @param project The project
   * @return The project, as a node
   */
  def projectToNode( project: Project[ _ ] ) = {
    <Project>
      { sheetsToNode( project.sheets.values.toList ) }
      { definitionsToNode( project.langs ) }
      { associationsToNode( project.associations ) }
    </Project>
  }

  /**
   * Writes the given project out to the given file
   * @param project The file to write to
   * @param file The file to write to
   * @throws ProjectWriteException If a format-level writing error occurred
   * @throws IOException If an error occurred on write
   */
  def writeProject( project: Project[ _ ],
		    file: File ) {
    import java.io._
    import sentinel.model.writer.xml._
    val printer = new PrettyPrinter( XMLWriter.WIDTH,
				     XMLWriter.INDENTATION )
    val asString = printer.format( projectToNode( project ) )
    val writer = new BufferedWriter( new FileWriter( file ) )
    writer.write( asString )
    writer.close()
  }
}

