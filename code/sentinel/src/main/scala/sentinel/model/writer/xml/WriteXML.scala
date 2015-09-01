/*
 * WriteXML.scala
 *
 * Version:
 *     $Id: WriteXML.scala,v 1.9 2011/06/08 04:27:25 kyledewey Exp $
 *
 * Revisions:
 *      $Log: WriteXML.scala,v $
 *      Revision 1.9  2011/06/08 04:27:25  kyledewey
 *      Conforms to the new interface for spreadsheet variables.
 *
 *      Revision 1.8  2011/04/10 04:06:28  kyledewey
 *      Append now works correctly.
 *
 *      Revision 1.7  2011/02/12 02:50:47  kyledewey
 *      Now uses SpreadsheetVariable instead of just Variable.
 *
 *      Revision 1.6  2010/07/11 05:52:47  kyledewey
 *      Formal parameters are no longer sorted upon being written
 *      out; this is mostly for show.
 *
 *      Revision 1.5  2010/06/25 03:20:25  kyledewey
 *      Refactored so that variables have types.
 *
 *      Revision 1.4  2010/06/20 22:54:50  kyledewey
 *      Now capable of writing out user-defined classes.
 *
 *      Revision 1.3  2010/06/20 17:32:39  kyledewey
 *      Now uses InstanceFactory[ _ ] instead of
 *      Either[ MatcherFactory, ReplacerFactory ].
 *
 *      Revision 1.2  2010/06/18 19:37:35  kyledewey
 *      Made factories take a name and description.
 *
 *      Revision 1.1  2010/06/18 03:00:12  kyledewey
 *      Initial revision
 *
 *
 */

package sentinel.model.writer.xml

import sentinel.model._
import sentinel.model.writer._
import scala.xml._

/**
 * Contains helper routines for XMLWriter.
 * @author Kyle Dewey
 */
object XMLWriter {
  // begin constants
  val WIDTH = java.lang.Integer.MAX_VALUE
  val INDENTATION = 2
  // constants

  /**
   * Given a constant, packages it as an XML node.
   * @param name The name of the constant
   * @param constant The constant to package
   * @return The constant as an XML node
   */
  def constantToNode( name: String, constant: Param ) = {
    <ConstantParameter>
      <Name>{ name }</Name>
      <Type>{ ParamType.toString( constant.getType ) }</Type>
      <Value>{ constant.sentStringValue }</Value>
    </ConstantParameter>
  }

  /**
   * Given a variable, packages it as an XML node.
   * @param name The name of the variable
   * @param variable The variable to package
   * @return The variable as an XML node
   */
  def variableToNode( name: String, variable: SpreadsheetVariable ) = {
    import Spreadsheet._
    <VariableParameter>
      <Name>{ name }</Name>
      <Type>{ ParamType.toString( variable.getType ) }</Type>
      <Sheet>{ variable.sheet.getOrElse( ANY_SHEET ) }</Sheet>
      <Row>{ variable.row.getOrElse( ANY_ROW ) }</Row>
      <Column>{ variable.column.getOrElse( ANY_COLUMN ) }</Column>
    </VariableParameter>
  }

  /**
   * Given a terminal node, packages it as either a constant or
   * a variable.
   * @param node The terminal node
   * @return The node as an XML node, either "ConstantParameter" or
   *         "VariableParameter"
   * @throws ClassWriteException If the node was a terminal, but didn't
   *         map to either a constant or a variable
   */
  def terminalNodeToNode( node: TerminalNode ) = {
    node.item match {
      case item: SimpleVariable[ _ ] => constantToNode( node.name, node.item )
      case item: Constant[ _ ] => constantToNode( node.name, node.item )
      case item: SpreadsheetVariable => variableToNode( node.name, item )
      case _ =>
	throw new ClassWriteException( "Unknown terminal kind in terminal " +
				       "node.  Name: " + node.name + "; " +
				       "Type: " + 
				       ParamType.toString( node.item.getType ) )
    }
  }

  /**
   * Packages a VariableNode as an XML node.
   * @param node The node to package
   * @return The node as an XML node
   */
  def variableNodeToNode( node: VariableNode ) = {
    <GivenParameter>
      <Name>{ node.name }</Name>
      <MapsTo>{ node.mapsTo }</MapsTo>
    </GivenParameter>
  }

  /**
   * Packages an internal node as an XML node.
   * @param node The node to package
   * @return The node as an XML node
   */
  def internalNodeToNode( node: InternalNode ): Node = {
    <InstanceParameter>
    <Name>{ node.name }</Name>
    <Type>{ ParamType.toString( node.factory.instanceType ) }</Type>
    <ClassName>{ node.factory.name }</ClassName>
    <ActualParameters>
    { node.getChildren.map( actualParameterToNode( _ ) ) }
    </ActualParameters>
    </InstanceParameter>
  }

  /**
   * Packages the given node representing an actual parameter to
   * a factory as an XML node
   * @param node The node to package
   * @return The node as an XML node
   * @throws ClassWriteException If a terminal node contained a non-terminal
   *         type, or if the node type is unknown
   */
  def actualParameterToNode( node: ParseNode ) = {
    node match {
      case node: TerminalNode => terminalNodeToNode( node )
      case node: VariableNode => variableNodeToNode( node )
      case node: InternalNode => internalNodeToNode( node )
      case _ =>
	throw new ClassWriteException( "Unknown parse node type with class: " +
				       node.getClass.getName )
    }
  }

  /**
   * Packages the parse tree structure of a class as a Structure node.
   * @param root The root of the parse tree
   * @return A Node encapsulating the parse tree structure
   */
  def parseTreeToNode( root: ParseNode ) = {
    <Structure>
    { actualParameterToNode( root ) }
    </Structure>
  }


  /**
   * Packages the given formal parameter as a node.
   * @param param The formal parameter
   * @return A Node encapsulating this parameter
   */
  def formalParamToNode( param: ParamInfo ) = {
    <FormalParameter>
    <Name>{ param.name }</Name>
    <Description>{ param.desc }</Description>
    <Type>{ ParamType.toString( param.paramType ) }</Type>
    <IsArray>{ param.isArray.toString }</IsArray>
    <IsRequired>{ param.isRequired.toString }</IsRequired>
    </FormalParameter>
  }
  
  /**
   * Packages the given formal parameters into nodes.
   * @param params The formal parameters to package
   * @return A node containing all formal parameters
   */
  def formalParamsToNode( params: Seq[ ParamInfo ] ) = {
    <FormalParameters>
    { params.map( formalParamToNode( _ ) ) }
    </FormalParameters>
  }

  /**
   * Packages the given class into a node.
   * @param theClass The class itself
   * @return The class, as a node
   * @throws ClassWriteException If an error occurred on converting the
   * class to XML
   */
  def classToNode( theClass: InstanceFactory[ _ ] ): Node = {
    val ( builtIn,
	  className ) = InstanceFactory.builtInInfo( theClass )
    try {
      <Class>
      <Name>{ theClass.name }</Name>
      <Description>{ theClass.desc }</Description>
      <Type>{ ParamType.toString( theClass.instanceType ) }</Type>
      <BuiltIn>{ builtIn.toString }</BuiltIn>
      { if ( builtIn ) 
	<JVMClassName>{ className.get }</JVMClassName>
	else Nil }
      { formalParamsToNode( theClass.validParams.values.toList ) }
      { if ( !builtIn )
	parseTreeToNode( theClass.asInstanceOf[ ParseTreeFactory[ _ ] ].tree )
        else Nil }
      </Class>
    } catch {
      case e: ClassWriteException => throw e
      case e: ClassCastException => 
	throw new ClassWriteException( "Class declared itself as a non built " +
				       "in; Name: " + theClass.name + "; " +
				       "Message: " + e.getMessage )
    }
  }

  /**
   * Packages all the given classes into nodes
   * @param classes The classes to package
   * @return All the classes as nodes
   */
  def classesToNodes( classes: Seq[ InstanceFactory[ _ ] ] ) =
    classes.map( classToNode( _ ) )

  /**
   * Packages the given classes into a node.
   * @param classes The classes to package
   * @return The classes underneath one node
   */
  def classesToNode( classes: Seq[ InstanceFactory[ _ ] ] ) = 
    classNodesToNode( classesToNodes( classes ) )

  /**
   * Packages the given class nodes as a single classes node
   * @param classes The classes to package
   * @return The classes in a single Classes Node
   */
  def classNodesToNode( classes: Seq[ Node ] ) = {
    <Classes>
    { classes }
    </Classes>
  }

  /**
   * Given previously written class nodes and factories that
   * have yet to be written, it will return a single Classes node
   * that contains them all.
   * @param previous Previously written classes
   * @param factories Factories that must be written out
   * @return A homogeneous seq holding the previous classes and the factories
   */
  def makeClassesNode( previous: Seq[ Node ], 
		       factories: Seq[ InstanceFactory[ _ ] ] ) = {
    classNodesToNode( previous ++ classesToNodes( factories ) )
  }

  /**
   * Writes the given classes to the given file.
   * @param classes The classes to write out
   * @param fileName The name of the file
   * @param append If we should append or clobber the file
   * @throws ClassWriteException If it couldn't figure out how to
   *         write out a given class
   * @throws IOException If an error occurred on writing
   */
  def writeClasses( classes: Seq[ InstanceFactory[ _ ] ],
		    fileName: String,
		    append: Boolean ) {
    new XMLWriter( fileName, append ).writeClasses( classes )
  }
  
  /**
   * Writes the given class to the given file.
   * @param theClass The class to write out
   * @param fileName The name of the file
   * @param append If we should append or clobber the file
   * @throws ClassWriteException If it couldn't figure out how to
   *         write out a given class
   * @throws IOException If an error occurred on writing
   */
  def writeClass( theClass: InstanceFactory[ _ ],
		  fileName: String,
		  append: Boolean ) {
    writeClasses( Seq[ InstanceFactory[ _ ] ]( theClass ),
		  fileName,
		  append )
  }
}

/**
 * Writes out classes as XML.
 * @param fileName The file name to write to.
 * @param append If we should append to the file instead of clobbering it
 * @param width The width to put the output to
 * @param indentation The number of spaces to use for indentation
 * @author Kyle Dewey
 */
class XMLWriter( fileName: String, 
		 val append: Boolean,
		 val width: Int,
	         val indentation: Int ) extends ClassWriter( fileName ) {
  import XMLWriter._
  private val printer = new PrettyPrinter( width,
					   indentation )
  
  /**
   * Creates a new writer with a width of
   * WIDTH and an indentation of INDENTATION.
   * @param fileName The name of the file to write to
   * @param append If we should append
   */
  def this( fileName: String, append: Boolean ) = 
    this( fileName, 
	  append,
	  XMLWriter.WIDTH,
	  XMLWriter.INDENTATION )
  
  /**
   * Creates a new writer with the default width and indentation
   * that doesn't append (clobbers)
   * @param fileName The name of the file to write to
   */
  def this( fileName: String ) =
    this( fileName, false )

  /**
   * Writes all of the given classes to a file.
   * @param classes The classes to write out
   * @throws IOException If an error occurred on writing
   */
  def writeClasses( classes: Seq[ InstanceFactory[ _ ] ] ) = {
    import java.io._
    val additional = additionalClasses
    val writer = new BufferedWriter( new FileWriter( fileName ) )
    val output = 
      printer.format( makeClassesNode( additional,
				       classes ) )
    writer.write( output )
    writer.close()
  }

  /**
   * Gets all "additional" classes.
   * If we were set to append, then this will return all classes in
   * the original file, in order.  If we weren't set to append, this returns
   * an empty Seq.
   * @return Either additional classes or Nil, as described above.
   */
  def additionalClasses() = {
    import sentinel.model.parser.xml._
    if ( !append ) {
      Nil
    } else {
      try {
	new XMLParser( fileName ).classes
      } catch {
	case e: Exception => Nil
      }
    }
  }
}
