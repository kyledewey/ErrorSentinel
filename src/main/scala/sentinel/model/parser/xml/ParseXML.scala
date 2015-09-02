/*
 * ParseXML.scala
 */

package sentinel.model.parser.xml

import sentinel.model._
import sentinel.model.parser._
import sentinel.model.matcher._
import sentinel.model.replacer._
import java.io._
import scala.xml._

/**
 * Contains a number of helper routines for ParseXML.
 * @author Kyle Dewey
 */
object XMLParser {
  import sentinel.model.ParamType._
  import sentinel.model.InstanceFactory._
  import sentinel.model.parser.ClassParser._

  def getNodes( base: Node, tag: String ) =
    XMLHelpers.getNodes( base, tag, ClassParseException( _ ) )

  def getText( base: Node, tag: String ) =
    XMLHelpers.getText( base, tag, ClassParseException( _ ) )

  /**
   * Given a formal parameter, parses it.
   * @param param the "FormalParameter" element
   * @return The formal parameter
   * @throws ClassParseException If a tag was missing, or if the
   *         text of one of the tags was malformed
   */
  def parseFormalParam( param: Node ): ParamInfo = 
    new ParamInfo( getText( param, "Name" ),
		   getText( param, "Description" ),
		   textToParamType( getText( param, "Type" ) ),
		   textToBoolean( getText( param, "IsArray" ) ),
		   textToBoolean( getText( param, "IsRequired" ) ) )

  /**
   * Given a listing of formal parameters, parses them into a map.
   * @param params the "FormalParameters" element
   * @return The class parameters, both in a map and in a sequence
   * based on the order they are in.
   * @throws ClassParseException if a tag was missing or the text malformed
   */
  def parseFormalParams( params: Node ) = {
    var map: Map[ String, ParamInfo ] = Map()
    var seq: Seq[ String ] = Seq()

    ( params \ "FormalParameter" ).foreach( param => {
      val current = parseFormalParam( param )
      map += (current.name -> current)
      seq ++= Seq( current.name )
    })

    (map, seq)
  }

  /**
   * Parses a given parameter node.
   * @param given The XML node backing the given parameter
   * @return a parse tree node correlating to the given parameter
   * @throws ClassParseException If a tag was missing
   */
  def parseGivenParameter( given: Node ) =
    new VariableNode( getText( given, "Name" ),
		      getText( given, "MapsTo" ) )
  
  /**
   * Parses a given constant parameter node.
   * @param constant The XML node backing the constant
   * @return A parse tree node correlating to the constant
   * @throws ClassParseException If a tag was missing, or if there was
   *         a type mismatch
   */
  def parseConstantParameter( constant: Node ) = {
    import sentinel.model.ParamType._
    val typeText = getText( constant, "Type" )
    val theType = stringToParam( typeText )
    val value = getText( constant, "Value" )
    try {
      val actualConstant = new StringConstant( value ).convertTo( theType ).get
      new TerminalNode( getText( constant, "Name" ), actualConstant )
    } catch {
      case e: NoSuchElementException => 
	throw new ClassParseException( "Constant type mismatch: expected " +
				       theType + "; found " +
				       ParamType.toString( looksLike( value ) ) )
    }
  }

  /**
   * Parses a given variable parameter node.
   * @param variable The XML node backing the variable
   * @return A parse tree node correlating to the variable
   * @throws ClassParseException If a tag was missing
   */
  def parseVariableParameter( variable: Node ) = {
    val varType = getText( variable, "Type" )
    val theVar = 
      new SpreadsheetVariable( textToSpreadsheet( getText( variable, "Sheet" ) ),
			       textToRow( getText( variable, "Row" ) ),
			       textToColumn( getText( variable, "Column" ) ) )
    new TerminalNode( getText( variable, "Name" ), theVar )
  }
  
  /**
   * Given the type and class name, will return a factory
   * correlating to the information.  If such a factory
   * doesn't exist, then it will throw an exception
   * @param typeName The name of the type, either "Matcher" or "Replacer"
   * @param className The name of the class to create
   * @throws ClassParseException If the type or class name is invalid
   */
  def getFactory( typeName: String, className: String ) = {
    if ( typeName == "Matcher" ) {
      validateMatcher( className )
      MatcherFactoryManager.getFactory( className ).get
    } else if ( typeName == "Replacer" ) {
      validateReplacer( className )
      ReplacerFactoryManager.getFactory( className ).get
    } else {
      throw new ClassParseException( "Unknown instance type with name: \"" +
				     typeName + "\"" )
    }
  }

  /**
   * Parses an instance node.  Note that it does so NON-RECURSIVELY.
   * @param instance XML node backing the instance
   * @return a parse tree node correlating to the instance
   * @throws ClassParseException If the type or class name is invalid,
   *         or if a neccessary tag was missing
   */
  def parseInstanceParameter( instance: Node ) = {
    val name = getText( instance, "Name" )
    val myType = getText( instance, "Type" )
    val className = getText( instance, "ClassName" )
    val factory = getFactory( myType, className )
    new InternalNode( name, factory )
  }

  /**
   * Recursively parses any kind of parameter.  Terminates when
   * there are no more sub parameters.
   * @param param XML node backing the parameter
   * @return A node representing the parameter.  Note that the node
   *         will have child nodes representing sub parameters.
   * @throws ClassParseException If an error occurred during parsing
   */
  def parseParameter( param: Node ): ParseNode = 
    param.label match {
      // base cases
      case "GivenParameter" => parseGivenParameter( param )
      case "ConstantParameter" => parseConstantParameter( param )
      case "VariableParameter" => parseVariableParameter( param )

      // recursive case
      // note that if there are no parameters to an instance,
      // this also acts as a base case
      case "InstanceParameter" => {
	val node = parseInstanceParameter( param )
	val currentParams = ( param \ "ActualParameters" ) \ "_"
	currentParams.foreach( newParam =>
	  node.attach( parseParameter( newParam ) ) )
	node
      }

      // error case
      case _ => 
	throw new ClassParseException( "Unknown parameter kind: \"" +
				       param.label + "\"" )
    }
  
  /**
   * Parses the structure of a class.  This is relevant to user-defined
   * classes.
   * @param structure The XML element containing the structure
   * @param name The name of the factory
   * @param desc A description of the factory
   * @param paramsMap What parameters are valid for the underlying object
   * @param paramsOrder The order of params
   * @param myType The type of the resulting structure
   * @return A factory that can create an element with this structure
   * @throws ClassParseException If an error was discovered in the parse tree
   */
  def parseStructure( structure: Node, 
		      name: String,
		      desc: String,
		      paramsMap: Map[ String, ParamInfo ],
		      paramsOrder: Seq[ String ],
		      myType: String ): InstanceFactory[ _ ] = {
    val children = structure \ "_"
    if ( children.size != 1 ) {
      // because this needs a custom error message,
      // getNodes() is not appropriate
      throw new ClassParseException( "More than one parameter in structure" )
    } 

    val tree = parseParameter( children.head )
    if ( myType == "Matcher" ) {
      MatcherFactory( name,
		      desc,
		      paramsMap,
		      paramsOrder,
		      tree )
    } else if ( myType == "Replacer" ) {
      ReplacerFactory( name,
		       desc,
		       paramsMap,
		       paramsOrder,
		       tree )
    } else {
      throw new ClassParseException( "Unknown instance type with name: \"" +
				     myType + "\"" )
    }
  }
      
  /**
   * Parses a single class.
   * @param theClass XML element containing the whole class
   * @return The class
   * @throws ClassParseException if there was some error, such as a tag missing
   *         or being of the inappropriate value
   */
  def parseClass( theClass: Node ): InstanceFactory[ _ ] = {
    val name = getText( theClass, "Name" )

    try {
      val desc = getText( theClass, 
			  "Description" )
      val theType = getText( theClass, 
			     "Type" )
      val builtIn = textToBoolean( getText( theClass, 
					    "BuiltIn" ) )
      val ( paramsMap,
	    paramsSeq ) = parseFormalParams( getNodes( theClass, 
					               "FormalParameters" ).head )

      if ( builtIn ) {
	val className = getText( theClass, "JVMClassName" )
	if ( theType == "Matcher" ) {
	  MatcherFactory( name,
			  desc,
			  paramsMap,
			  paramsSeq,
			  className )
	} else if ( theType == "Replacer" ) {
	  ReplacerFactory( name,
			   desc,
			   paramsMap,
			   paramsSeq,
			   className )
	} else {
	  throw new ClassParseException( "Unknown instance type with name: \"" +
					 theType + "\"" )
	}
      } else {
	val structure = getNodes( theClass, "Structure" ).head
	parseStructure( structure,
		        name,
		        desc,
		        paramsMap,
		        paramsSeq,
		        theType )
      }
    } catch {
      case e: Exception => 
	throw new ClassParseException( "Error in class definition: " +
				       "\"" + name + "\": " +
				       e.toString )
    }
  } // parseClass

  /**
   * Given a class node, returns a pre class node that correlates to
   * it.  Note that dependency information will not be included.
   * @param node The node correlating to the class
   * @param parser The parser that they came from
   * @return A pre-class node detailing this class, with the exception
   * of dependencies
   * @throws ClassParseException If this information was missing or
   * otherwise malformed
   */
  def toPreClass( node: Node, parser: ClassParser ) =
    PreClass( getText( node, "Name" ),
	      textToInstanceType( getText( node, "Type" ) ),
	      parser,
	      dependencies( node ) )
  
  /**
   * Given a class node, returns the names and types of all the classes
   * that this node depends on.  Note that only unique dependencies are
   * listed
   * @param node The node correlating to the class
   * @return A sequence of pairs, where each pair is of the name and type
   * of a dependency
   * @throws ClassParseException If some information was missing or malformed
   */
  def dependencies( node: Node ): Seq[ (String, ParamType) ] =
    ( Set() ++ ( node \\ "InstanceParameter" ).map( current =>
      (getText( current, "ClassName" ),
       textToInstanceType( getText( current, "Type" ) )) ) ).toSeq
  
  /**
   * Given a bunch of classes nodes, returns a bunch of pre-class objects to
   * which they correlate.
   * @param classes The classes nodes
   * @param parser The parser that they came from
   * @return A parallel sequence of pre-class objects
   * @throws ClassParseException If a format-level error is present
   */
  def parseForPreClasses( classes: Seq[ Node ],  parser: ClassParser ) =
    classes.map( toPreClass( _, parser ) )
} // XMLParser

/**
 * Parses an XML file.
 * @param fileName The name of the file
 * @throws FileNotFoundException If the given file could not be opened
 * @throws IOException If a read was unsuccessful
 * @author Kyle Dewey
 */
class XMLParser( fileName: String ) extends ClassParser( fileName ) {
  import sentinel.model.ParamType._
  val file = XML.loadFile( fileName )
  val classes = file \ "Class"
  // maps pre-class objects to nodes that contain class information
  lazy val preClassToNode =
    Map() ++ XMLParser.parseForPreClasses( classes, this ).zip(classes.toList)
  
  /**
   * Gets information about all classes within
   * @return PreClass information about all classes within
   * @throws ClassParseException If there was a format-level error
   */
  override def classesInformation() = 
    preClassToNode.keys.toList

  /**
   * Parses in the given class
   * @param theClass The class to parse in
   * @throws ClassParseException If there was a format-level error, or the
   * class doesn't exist
   * @throws IOException If an error occurred on reading
   */
  override def internalParseClass( theClass: PreClass ) = {
    if ( preClassToNode.contains( theClass ) ) {
      XMLParser.parseClass( preClassToNode( theClass ) )
    } else {
      throw new ClassParseException( "Unknown class: " + theClass )
    }
  }
}
