/*
 * Node.scala
 */

package sentinel.vpl

import sentinel.model._
import java.awt.Color

/**
 * Holds constants for nodes
 * @author Kyle Dewey
 */
object Node {
  // special string representing the output of a node
  val OUTPUT_LINE = "output"

  // colors for nodes
  val CONNECTED_INPUT_COLOR = Color.GREEN
  val UNCONNECTED_INPUT_COLOR = Color.RED
  val CONNECTED_OUTPUT_COLOR = Color.BLACK
  val UNCONNECTED_OUTPUT_COLOR = Color.BLACK

  // error for when the user attempts to make a cycle
  val CYCLE_ERROR = "Given connection would form a cycle."

  val DEFAULT_NODE_HEIGHT = 22 // node height
  val DEFAULT_NODE_WIDTH_PER = 40 // node width per input
}

/**
 * Holds constants for nodes in sentinel.
 * @author Kyle Dewey
 */
object SentinelNode {
  // additional colors
  val UNCONNECTED_OPTIONAL_INPUT_COLOR = Color.BLACK

  // debugger class name
  val DEBUGGER_CLASS = "sentinel.model.replacer.Debugger"

  /**
   * Determines if the given instance factory is a debugger.
   * @param factory The factory to check
   * @return true if it is a debugger, else false
   */
  def isDebugger( factory: InstanceFactory[ _ ] ) =
    factory match {
      case f: ReflectionFactory[ _ ] if ( f.JVMClassName == DEBUGGER_CLASS ) => true
      case _ => false
    }

  /**
   * Creates a new sentinel node.
   * Needed since debuggers are a special case.
   * @param factory The factory for the node
   * @return A new sentinel node, it's actual type dependent upon
   * the factory given
   */
  def apply( factory: InstanceFactory[ _ ] ) =
    if ( isDebugger( factory ) ) {
      new SentinelDebuggingNode( factory )
    } else {
      new SentinelNode( factory )
    }

  /**
   * Converts the given boolean value to "Yes" or "No", depending on
   * whether it is true or false.
   * @param value The boolean value to convert
   * @return "Yes" for true, or "No" for false
   */
  def toYesNo( value: Boolean ) =
    if ( value ) "Yes" else "No"

  /**
   * Given a factory, returns the number for each input, as determined
   * by paramOrder
   * @param factory The factory to get the input numbers of
   * @return A mapping of input names to their numbers
   */
  def inputNameToNumber( factory: InstanceFactory[ _ ] ) =
    Map() ++ 0.until( factory.paramOrder.length )
              .map( num => (factory.paramOrder( num ), num) )
}

/**
 * Exception thrown when an attempt is made to connect incompatible nodes.
 * @param message A message describing why the connection is invalid
 * @author Kyle Dewey
 */
case class InvalidNodeConnectionException( message: String ) extends Exception( message ) {}

/**
 * Exception thrown when an attempt is made to manipulate a node we
 * don't recognize.
 * @param message A message describing the problem
 * @author Kyle Dewey
 */
case class UnknownNodeException( message: String ) extends Exception( message ) {}

/**
 * Exception thrown when an input value to a node isn't valid.
 * @param message A message to show the user
 * @param input The input that was invalid
 * @param node The node that triggered the error
 * @author Kyle Dewey
 */
case class InputException[ T <: AnyRef, U ]( message: String, 
					     val input: String,
					     val node: Node[ T, U ] ) extends Exception( message ) {
  /**
   * Creates an input exception based on another exception.
   * Merely uses the message of the other exception.
   * @param exception The underlying exception
   * @param input The name of the input
   * @param node The node that couldn't instantiate
   */
  def this( exception: Exception, input: String, node: Node[ T, U ] ) =
    this( exception.getMessage, input, node )
}

/**
 * Represents a node in the graph.
 * Nodes can be connected to other nodes.  Input and output types are based
 * upon the given instance factory.
 *
 * <p>Note that these are only loosely tied to the rest of sentinel.  Nodes
 * assume that there are 0-n names inputs on components, along with a single
 * output.  They allow for connection of outputs to inputs, and can understand
 * configurable rules for other connections (i.e. real number outputs can
 * only be connected to real number inputs).  The intention is that this can
 * be used outside of sentinel in the development for other VPLs.</p>
 *
 * <p>Nodes hold components, which can be anything.  It is intended that components
 * are somewhat related to the given node.  inputConnections maps input names
 * to nodes.  output gets what node we are connected to.</p>
 *
 * <p>Note that it is intended that the keys of <code>inputConnections</code> will never
 * change, although the values might.</p>
 *
 * @param component A component to hold on the node
 * @param inputConnections A mapping of input names to nodes that they
 * are connected to
 * @param inputNumber A mapping of input names to which numbers they are.
 * The numbers should be of the form 0, 1, 2, ..., n.
 * @param output What node our output is connected to, along with the name
 * of the input that we are connected to 
 * @author Kyle Dewey
 */
abstract class Node[ T <: AnyRef, U ]( val component: T,
				       var inputConnections: Map[ String, Seq[ Node[ T, U ] ] ],
				       val inputNumber: Map[ String, Int ],
				       var output: Option[ (String, Node[ T, U ]) ] ) extends Describable {
  val inputNumberToName = Map() ++ inputNumber.map( 
    ( pair: (String, Int) ) => (pair._2, pair._1) )

  /**
   * Creates a new node that isn't connected to anything, with the given
   * mapping of input names to their input numbers
   * @param component The component to use
   * @param names The names of all the inputs, and their mapping to integers
   */
  def this( component: T, names: Map[ String, Int ] ) =
    this( component,
	  Map() ++ names.keys.toList.map( s => (s, Seq[ Node[ T, U ] ]()) ),
	  names,
	  None )

  /**
   * Gets the width of this node
   * @return The width of this node
   */
  def width() = {
    val num = 
      if ( numInputs == 0 ) 1 else numInputs
    num * Node.DEFAULT_NODE_WIDTH_PER
  }

  /**
   * Gets the height of this node
   * @return the height of this node
   */
  def height() =
    Node.DEFAULT_NODE_HEIGHT

  /**
   * Gets the number of inputs that are connected.  For example, if we have two
   * inputs, and three things are attached to the first one and none on the
   * other, then this returns 1, signifying that one of the inputs lines is
   * connected.
   * @return The number of inputs that are connected
   */
  def numConnectedInputs() =
    inputNames.map( ( name: String ) => 
      if ( inputConnected( name ) ) 1 else 0 )
              .reduceLeft( _ + _ )

  /**
   * Gets the input with the given number
   * @param num The number of the input
   * @return The input, or None if it's not connected
   * @throws UnknownInputException If an input with the given number wouldn't
   * exist anyway
   */
  def input( num: Int ) = {
    verifyInputNum( num )
    inputConnections( inputNumberToName( num ) )
  }

  /**
   * Gets the names of all the inputs.
   * @return The names of all the inputs
   */
  def inputNames(): Seq[ String ] =
    inputConnections.keys.toList

  /**
   * Gets the number of inputs that are on this node
   * @return The number of inputs on this node
   */
  def numInputs() =
    inputConnections.size

  /**
   * Determines if the given name is understood to be an input name
   * @param name The name of the input
   * @return true if we understand this as an input name
   */
  def isInputName( name: String ) =
    inputConnections.contains( name )

  /**
   * Determines if the given input is connected
   * @param name the name of the input
   * @return true if the input is connected, else false
   * @throws UnknownInputException If the given input name isn't recognized
   */
  def inputConnected( name: String ): Boolean = {
    verifyInputName( name )
    !inputConnections( name ).isEmpty
  }

  /**
   * Determines if the given line is connected
   * @param The name of the line (or OUTPUT_LINE )
   * @return true if the line is connected, else false
   */
  def lineConnected( name: String ) =
    if ( name.eq( Node.OUTPUT_LINE ) ) {
      outputConnected
    } else {
      inputConnected( name )
    }
  
  /**
   * Gets the color of this node
   * @return The color of this node
   */
  def color() =
    Color.LIGHT_GRAY

  /**
   * Gets the color of the given input
   * @param num The number of the input
   * @return The color of the given input
   * @throws UnknownInputException If the given input number isn't recognized
   */
  def inputColor( num: Int ): Color = {
    verifyInputNum( num )
    if ( inputConnected( num ) ) {
      Node.CONNECTED_INPUT_COLOR
    } else {
      Node.UNCONNECTED_INPUT_COLOR
    }
  }

  /**
   * Gets the color of the output
   * @return The color of the output
   */
  def outputColor() = 
    if ( outputConnected ) {
      Node.CONNECTED_OUTPUT_COLOR
    } else {
      Node.UNCONNECTED_OUTPUT_COLOR
    }

  /**
   * Determines if the given input is connected
   * @param num The number of the input
   * @return true if the input is connected
   * @throws UnknownInputException If the given input number isn't recognized
   */
  def inputConnected( num: Int ): Boolean = {
    verifyInputNum( num )
    inputConnected( inputNumberToName( num ) )
  }

  /**
   * Gets all the ancestor nodes of the given node, traversing input lines.
   * Note that this will cause infinite recursion if there is a cycle in
   * the graph.  Also note that this includes <code>this</code> in the set
   * of nodes returned.
   * @pre There isn't a cycle in the graph
   * @return All the ancestor nodes of this node, via input lines
   */
  protected def internalAncestors(): Set[ Node[ T, U ] ] = 
    inputConnections.filterKeys( inputConnected( _ ) ) // get connected inputs
		    .values // get their sets
                    .foldLeft( Set[ Node[ T, U ] ]() )( _ ++ _ ) // into one set
                    .map( _.internalAncestors ) // get the ancestors of those
                    .foldLeft( Set( this ) )( _ ++ _ ) // concat it all
  
  /**
   * Gets all ancestors nodes of the given node, traversing input lines.
   * Note that this will cause infinite recursion if there is a cycle in
   * the graph.
   * @pre There isn't a cycle in the graph
   * @return All the ancestor nodes of this node, via input lines
   */
  def ancestors(): Set[ Node[ T, U ] ] =
    internalAncestors - this

  /**
   * Determines whether or not all the given nodes are ancestors of this
   * node.
   * @param nodes The nodes to test
   * @return true if they are all ancestors of this node, else false
   */
  def ancestors( nodes: Seq[ Node[ T, U ] ] ): Boolean = {
    val ancest = ancestors
    nodes.forall( ancest.contains( _ ) )
  }

  /**
   * Gets all child nodes of the given node, traversing output lines
   * Note that this will cause infinite recursion if there is a cycle in
   * the graph.  (Unlike ancestors, this is tail-recursive, so don't
   * expect a stack overflow exception.)
   * @pre There isn't a cycle in the graph
   * @return All the child nodes of this node, via output lines
   */
  def children(): Set[ Node[ T, U ] ] = {
    var retval: Set[ Node[ T, U ] ] = Set()

    def children( node: Node[ T, U ] ) {
      if ( node.output.isDefined ) {
	val current = node.output.get._2
	retval += current
	children( current )
      }
    }

    children( this )
    retval
  }

  /**
   * Gets the last child in the path of this node.
   * If this returns <code>this</code>, then this node is the last one.
   * @return The last child in this node's path
   */
  def lastChild(): Node[ T, U ] = {
    def lastChild( node: Node[ T, U ] ): Node[ T, U ] =
      if ( node.output.isEmpty ) {
	node
      } else {
	lastChild( node.output.get._2 )
      }
    lastChild( this )
  }
	
  /**
   * Determines if adding the given node would introduce a cycle.
   * This means that the given node is already amonst our ancestors
   * @param node The node to try to add
   * @return true if adding the node would introduce a cycle, else false
   */
  def wouldIntroduceCycle( node: Node[ T, U ] ) =
    node.eq( this ) || ( ancestors ++ children ).contains( node )

  /**
   * Verifies the given connection.  Throws an exception if the given
   * connection is invalid.
   * @param node The node to connect
   * @param input The name of the input
   * @throws InvalidNodeConnectionException If the given nodes cannot be
   * connected
   * @throws UnknownInputException If the given input name is unrecognized
   */
  def validConnection( node: Node[ T, U ], input: String ) {
    verifyInputName( input )

    if ( wouldIntroduceCycle( node ) ) {
      throw new InvalidNodeConnectionException( Node.CYCLE_ERROR )
    }
  }

  /**
   * Verifies that the given node is connected to the given input
   * @param node The node
   * @param input The name of the input
   * @throws UnknownInputException If the given input name is unrecognized
   * @throws UnknownNodeException If the given node isn't connected to the
   * given input
   */
  def verifyConnected( node: Node[ T, U ], input: String ) {
    verifyInputName( input )
    if ( !inputConnections( input ).contains( node ) ) {
      throw new UnknownNodeException( "Given node not connected to " + input )
    }
  }

  /**
   * Disconnects all inputs.
   */
  def disconnectInput() {
    inputConnections.keys.toList.foreach( disconnectInput( _ ) )
  }

  /**
   * Disconnects all nodes from the given input.
   * A no-op if there are no nodes connected on this input
   * @param input The name of the input
   * @throws UnknownInputException If the given input name is unrecognized
   */
  def disconnectInput( input: String ) {
    verifyInputName( input )
    if ( inputConnected( input ) ) {
      inputConnections( input ).foreach( disconnectInput( _, input ) )
    }
  }

  /**
   * Disconnects the given node from the given input.
   * @param node The node to disconnect
   * @param input The name of the input
   * @throws UnknownInputException If the given input name is unrecognized
   * @throws UnknownNodeException If the given node isn't connected to the
   * given input
   */
  def disconnectInput( node: Node[ T, U ], input: String ) {
    verifyConnected( node, input )
    node.output = None
    inputConnections += (input -> inputConnections( input ).filter( !_.eq( node ) ))
  }

  /**
   * Disconnects the output node
   */
  def disconnectOutput() {
    if ( output.isDefined ) {
      // node that disconnectInput ends up setting output to None
      output.get._2.disconnectInput( this, output.get._1 )
    }
  }

  /**
   * Disconnects the given line.
   * @param line The name of the line to disconnect (an input name or
   * OUTPUT_LINE)
   */
  def disconnectLine( line: String ) {
    if ( line.eq( Node.OUTPUT_LINE ) ) {
      disconnectOutput()
    } else {
      disconnectInput( line )
    }
  }

  /**
   * Synonym for disconnectLine.
   * @param line The name of the line to disconnect
   */
  def disconnect( line: String ) {
    disconnectLine( line )
  }

  /**
   * Disconnects this node from the rest of the nodes.
   * That is, it disconnects both inputs and the output
   */
  def disconnect() {
    disconnectInput()
    disconnectOutput()
  }

  /**
   * Connects the given node's output to the given input
   * @param node The node to connect
   * @param input The name of the input to connect to
   * @throws InvalidNodeConnectionException If the given nodes cannot
   * be connected
   */
  def connect( node: Node[ T, U ], input: String ) {
    validConnection( node, input )

    // add the new input connection
    inputConnections += (input -> (inputConnections( input ) ++ Seq( node )))

    // set the node's output
    node.disconnectOutput()
    node.output = Some( (input, this) )
  }

  /**
   * Determines if the output is connected to anything.
   * @return <code>output.isDefined</code>
   */
  def outputConnected() =
    output.isDefined

  /**
   * Verifies that the given input name is known
   * @param input The name of the input to verify
   * @throws UnknownInputException If the given input name is unrecognized
   */
  def verifyInputName( input: String ) {
    if ( !inputConnections.contains( input ) ) {
      throw new UnknownInputException( "Unrecognized input name: " + input )
    }
  }

  /**
   * Verifies that the given input number is known
   * @param num The number of the input to verify
   * @throws UnknownInputException If the given input number is unrecognized
   */
  def verifyInputNum( num: Int ) {
    if ( !inputNumberToName.contains( num ) ) {
      throw new UnknownInputException( "Unrecognized input number: " + num )
    }
  }

  /**
   * Gets what the return value of this node is.
   * By default, this always returns None.
   * @return The value of this node.  Returns None if we cannot
   * generate a value for whatever reason.
   */
  def returnValue(): Option[ U ] = None
}

/**
 * Represents a node whoose component is in and of itself
 * describable.  Will use that component as the description item.
 * @param component A component to hold on the node
 * @param inputConnections A mapping of input names to nodes that they
 * are connected to
 * @param inputNumber A mapping of input names to which numbers they are.
 * The numbers should be of the form 0, 1, 2, ..., n.
 * @param output What node our output is connected to, along with the name
 * of the input that we are connected to 
 * @author Kyle Dewey
 */
class DescribableNode[ T <: Describable, U ]( component: T,
					      inputConnections: Map[ String, Seq[ Node[ T, U ] ] ],
					      inputNumber: Map[ String, Int ],
					      output: Option[ (String, Node[ T, U ]) ] ) 
extends Node[ T, U ]( component, inputConnections, inputNumber, output ) {
  /**
   * Creates a new node that isn't connected to anything, with the given
   * mapping of input names to their input numbers
   * @param component The component to use
   * @param names The names of all the inputs, and their mapping to integers
   */
  def this( component: T, names: Map[ String, Int ] ) =
    this( component,
	  Map() ++ names.keys.toList.map( s => ( s, Seq[ Node[ T, U ] ]()) ),
	  names,
	  None )
  
  /**
   * Gets the component, which can be used to describe the node.
   * @return <code>component</code>.
   */
  def describer() =
    component.describer
}
								    
/**
 * A node that is specific to sentinel.
 * Merely overrides the description methods with more useful and specific vairants.
 * @param factory The matcher/replacer factory behind the given node
 * @author Kyle Dewey
 */
class SentinelNode( val factory: InstanceFactory[ _ ] )
extends DescribableNode[ InstanceFactory[ _ ], Param ]( factory, SentinelNode.inputNameToNumber( factory ) ) {
  val inputRequired = 
    factory.paramOrder.map( factory.validParams( _ ).isRequired )
                      .toArray

  /**
   * Converts this node to a parse tree node.
   * @param name The name of the internal node
   * @return An internal node representation
   */
  def parseTreeNode( name: String ): ParseNode =
    new InternalNode( name, factory )

  /**
   * Names all parameters with the given name.
   * @param name The name of the parameter.
   * @return parameters correlating to the name.  If there are none, then
   * the given input doesn't exist, or it's not a SentinelNode, or it's None.
   * Otherwise, it returns a named param for each input.
   * @throws InputException If an input isn't valid
   */
  def nameParams( name: String ): Seq[ NamedParam ] =
    if ( !inputConnections.contains( name ) ) {
      Seq[ NamedParam ]()
    } else {
      inputConnections( name ).filter( _.isInstanceOf[ SentinelNode ] )
                              .map( _.asInstanceOf[ SentinelNode ].returnValue )
                              .filter( _.isDefined )
                              .map( ( p: Option[ Param ] ) => 
				new NamedParam( name, p.get ) )
    }
  
  /**
   * Applies names to all parameters.
   * @return a sequence of named parameters, correlating to our input
   * connections.  Note that input nodes that are not SentinelNodes are
   * ignored, as are SentinalNodes with None as a return value.
   */
  def nameParams(): Seq[ NamedParam ] = 
    inputConnections.keys.map( nameParams( _ ) )
                         .foldLeft( Seq[ NamedParam ]() )( _ ++ _ )

  /**
   * Gets the return value of this node without checking to see if
   * it's possible.
   * Since the check needs to be performed only once, it is wasteful to
   * do it for all nodes.
   * @return The value of this node
   * @throws InputException If an input is invalid
   */
  protected def returnValueNoCheck(): Option[ Param ] =
    // grr...this is type safe
    // Scala isn't smart enough to see that the constraint on
    // instantiate that the return value must be a Param means that
    // the return value will be at least a Param
    try {
      Some( factory.instantiate( nameParams, false ).asInstanceOf[ Param ] )
    } catch {
      case e: ParameterTypeException => 
	throw new InputException( e, e.param.name, this )
    }
  

  /**
   * Gets the return value of this node.
   * If all the required parameters are in place, then it will try to make
   * a return value.  If not, then it will return none.
   * @return The value of this node, or None if we couldn't make a value
   * @throws InputException If an input is invalid
   */
  override def returnValue() = 
    if ( !returnValuePossible ) 
      None
    else 
      returnValueNoCheck
    
  /**
   * Gets whether or not all required inputs have been connected
   * for this particular node.
   * @return true if all required inputs are connected, else false
   */
  def requiredInputsConnected() =
    0.until( numInputs ).filter( inputRequired( _ ) )
                        .forall( inputConnected( _ ) )

  /**
   * Gets whether all required inputs have been connected in the entire
   * tree of ancestors.
   * @return true if all required inputs for ancestors are connected
   */
  def requiredInputsConnectedAncestors() =
    ancestors.filter( _.isInstanceOf[ SentinelNode ] )
             .map( _.asInstanceOf[ SentinelNode ] )
             .forall( _.requiredInputsConnected )

  /**
   * Gets whether or not it's possible to get a return value for this node
   * This means that all required inputs from this node above have
   * been connected.
   * @return true if it's possible to get a return value, else false
   */
  def returnValuePossible() =
    requiredInputsConnected && requiredInputsConnectedAncestors

  /**
   * Gets the color of the given input.
   * @param num The number of the input
   * @throws UnknownInputException If the given input number isn't recognized
   */
  override def inputColor( num: Int ): Color = {
    verifyInputNum( num )
    if ( !inputRequired( num ) && !inputConnected( num ) ) {
      SentinelNode.UNCONNECTED_OPTIONAL_INPUT_COLOR
    } else {
      super.inputColor( num )
    }
  }

  /**
   * Gets if the given node can be connected to this node's input.
   * In addition to performing the superclass' checks, this also checks
   * to see if an element is an array.  If it's not, then it will only
   * allow one connection.
   * @param node The node to try to connect
   * @param input The name of the input to connect to
   * @throws InvalidNodeConnectionException If the given nodes cannot be
   * connected
   * @throws UnknownInputException If the given input name is unrecognized
   */
  override def validConnection( node: Node[ InstanceFactory[ _ ], Param ], 
			        input: String ) {
    super.validConnection( node, input )

    if ( inputConnected( input ) &&
	 !factory.validParams( input ).isArray ) {
      throw new InvalidNodeConnectionException( "Given input does not accept " +
					        "mulitple inputs (isn't an " +
					        "array)" )
    }
  }

  /**
   * Simple toString method, used for debugging.
   * @return A string holding what the value of this node is
   */
  override def toString() = {
    var retval = super.toString
    val value = returnValue

    if ( returnValue.isDefined ) {
      val print = returnValue.get.printableValue
      retval += ": " + print.toString
    }
    retval
  }
}

/**
 * Contains helper routines for <code>SentinelDebuggingNode</code>.
 * @author Kyle Dewey
 */
object SentinelDebuggingNode {
  val DEFAULT_VARIABLE_VALUE = ""
  val INPUT_NAME = "input"
}

/**
 * A debugging node in sentinel.
 * Granted, this breaks a lot of the flow of things, but debuggers rarely
 * play by the rules.
 * @param factory The debugging factory that backs the node
 * @param variable Variable to use for updating the GUI.  This class will
 * change the value of the variable if we have an input and returnValue()
 * is called on us.  If we don't have a value and return value is called
 * on us, then we merely pass the value along without changing it.
 * @author Kyle Dewey
 */
class SentinelDebuggingNode( factory: InstanceFactory[ _ ],
			     val variable: SimpleStringVariable ) 
extends SentinelNode( factory ) {
  /**
   * Creates a new node, along with a new variable.
   * The variable's initial value is <code>DEFAULT_VARIABLE_VALUE</code>.
   * @param factory The debugging factory that backs the code.
   */
  def this( factory: InstanceFactory[ _ ] ) =
    this( factory,
	  new SimpleStringVariable( SentinelDebuggingNode.DEFAULT_VARIABLE_VALUE ) )

  /**
   * Gets the return value of this node.  If we have an input, it merely
   * passes along that value.  It will change the value of the variable to
   * reflect the new value.  If we don't have an input, it will use the
   * value of the variable as an output.  In this case, it won't change the
   * value of the variable.
   * @return The return value of the node, as described above
   */
   override def returnValueNoCheck() = {
     try {
       var retval: Option[ Param ] = None
       if ( !inputConnected( SentinelDebuggingNode.INPUT_NAME ) ) {
	 // used to pass along value
	 retval = Some( variable )
       } else {
	 // we need to show something
	 val params = nameParams
	 if ( params.isEmpty ) {
	   // input is invalid
	   variable.variable = SentinelDebuggingNode.DEFAULT_VARIABLE_VALUE
	   retval = Some( variable )
	 } else {
	   retval = Some( factory.instantiate( params, false ).asInstanceOf[ Param ] )
	   variable.variable = retval.get.printableValueUnsafe
	 }
       }
       retval
     } catch {
       case e: ParameterTypeException => 
	 throw new InputException( e, e.param.name, this )
     }
   }
}


