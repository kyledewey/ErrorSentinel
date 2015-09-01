/*
 * Function.scala
 *
 * Version:
 *     $Id: Function.scala,v 1.12 2011/06/21 10:08:25 kyledewey Exp $
 * 
 * Revisions:
 *      $Log: Function.scala,v $
 *      Revision 1.12  2011/06/21 10:08:25  kyledewey
 *      Parameter renaming now works properly.
 *
 *      Revision 1.11  2011/06/21 09:39:00  kyledewey
 *      If a parameter isn't marked as used, it is no longer
 *      included.
 *
 *      Revision 1.10  2011/06/20 22:19:56  kyledewey
 *      Debugging nodes whose values are used as constants
 *      are now written out as constants.
 *
 *      Revision 1.9  2011/06/19 20:05:51  kyledewey
 *      Refactored PreFunctionParams.
 *      PreFunctionParam code moved to PreFunctionParam.scala.
 *
 *      Revision 1.8  2011/05/31 00:04:31  kyledewey
 *      Moved moveSubsequence() to SentinelHelpers.
 *      Moved validateChangeParameterType() to sentinel.utils.interactive.
 *
 *      Revision 1.7  2011/05/27 18:50:48  kyledewey
 *      Moved the multiMap() method to sentinel.model.SentinelHelpers.
 *
 *      Revision 1.6  2011/04/04 00:21:30  kyledewey
 *      moveSubsequence() now works correctly.
 *
 *      Revision 1.5  2011/04/03 21:17:02  kyledewey
 *      Refactored how parameters are moved.
 *
 *      Revision 1.4  2011/04/03 18:59:14  kyledewey
 *      Fixed bug where parameter renaming wasn't
 *      performed in order.
 *
 *      Revision 1.3  2011/04/03 04:18:02  kyledewey
 *      Refactored so parameter order is easier to manipulate.
 *
 * 
 *  
 */

package sentinel.vpl

import sentinel.model._
import ParamType._

/**
 * Contains constants and static routines relevant to function creation.
 * @author Kyle Dewey
 */
object Function {
  import sentinel.model.SentinelHelpers._

  /**
   * Validates that the items seen in the given Seq are the same as those
   * in the given set.
   * @param seq The sequence
   * @param set The set
   * @return true if they hold the same items, else false
   */
  def same[ T ]( seq: Seq[ T ], set: Set[ T ] ): Boolean =
    ( Set() ++ seq ) == set

  /**
   * Same as <code>same</code>, but with a different parameter order.
   * @param set The set
   * @param seq The seq
   * @return <code>same( seq, set )</code>
   */
  def same[ T ]( set: Set[ T ], seq: Seq[ T ] ): Boolean =
    same( seq, set )

  /**
   * Validates that the given sequence of SentinelNodes contains
   * exactly the same nodes as seen in the given set.
   * @param seq The sequence of SentinelNodes
   * @param set The set of SentinelNodes
   * @throws NodeMismatchException If the nodes in selectedNodes at all differ
   * from the order of nodes
   */
  def validateSame( seq: Seq[ SentinelNode ], set: Set[ SentinelNode ] ) {
    if ( !same( seq, set ) ) {
      throw new NodeMismatchException( "Nodes in the set differ from those " +
				       "in the sequence." )
    }
  }

  /**
   * Like <code>getTailNodes</code>, but exactly one output is expected.
   * @param nodes The nodes that have been selected
   * @return The output node for a function
   * @throws BadOutputException If there is not exactly one possible output.
   */
  def getTailNode( nodes: Set[ SentinelNode ] ) = {
    val tails = getTailNodes( nodes )
    val length = tails.length
    if ( length != 1 ) {
      throw new BadOutputException( length )
    }
    tails.head
  }

  /**
   * Given a bunch of nodes, it will return nodes that either
   * have unconnected outputs or nodes that are on the border
   * of the nodes that have been connected.  In the second case,
   * consider the graph 1->2->3->4.  If this is given 1,2,3, then
   * this will return 3, as 3 is on the border of being selected.
   * This is intended for creating functions.
   * @param nodes The nodes to search through
   * @return The nodes that either have unconnected outputs or those
   * which are the last nodes before one goes outside of the selection
   */
  def getTailNodes( nodes: Set[ SentinelNode ] ) = {
    var seen: Set[ SentinelNode ] = Set()
    var retval: List[ SentinelNode ] = List()

    // recursively processes the given node through outputs
    def processNode( node: SentinelNode ) {
      if ( !seen.contains( node ) ) {
	// lazy because a node may not even have an output
	lazy val output = node.output.get._2.asInstanceOf[ SentinelNode ]
	seen += node
	if ( !node.outputConnected || // output not connected
	     !nodes.contains( output ) ) { // on edge
	  retval ::= node
	} else {
	  processNode( output )
	}
      }
    }

    nodes.foreach( processNode( _ ) )
    retval.reverse.toSeq
  }

  /**
   * Determines if a given node's input has any connections that are within
   * the given set.
   * @param node The node to check
   * @param inputName The name of the input to check
   * @param selectedNodes The nodes that have been selected.
   * @return True if there are any input connections to the given input that
   * are contained in selectedNodes.  If there aren't any input connections,
   * or all input connections are to nodes outside of selectedNodes, this
   * returns false
   */
  def inputConnectedTo( node: SentinelNode,
		        inputName: String,
		        selectedNodes: Set[ SentinelNode ] ) = {
    node.inputConnections( inputName )
        .map( _.asInstanceOf[ SentinelNode ] )
	.exists( selectedNodes.contains( _ ) )
  }

  /**
   * Determines if the given input should be treated as a potential
   * parameter.
   * @param node The node with the input
   * @param inputName The name of the input
   * @param selectedNodes The nodes that have been selected
   * @return true if it should be treated as a parameter, else false
   */
  def isPotentialParameter( node: SentinelNode,
			    inputName: String,
			    selectedNodes: Set[ SentinelNode ] ) = {
    !inputConnectedTo( node, 
		       inputName, 
		       selectedNodes )
  }

  /**
   * Given a node and the nodes that have been selected, it gets the names
   * of inputs that have not been connected.  Note that any nodes that
   * are outside of those nodes that have been selected are ignored.  I.e.
   * a node could be connected but if this connection isn't in the set of
   * selected nodes, then this connection is ignored and considered unconnected.
   * @param node The node to get unconnected inputs of
   * @param selectedNodes All the nodes that have been selected.  Any nodes
   * outside of this grouping are ignored.
   * @return The names of unconnected inputs, or inputs that have connections
   * to nodes not seen in <code>selectedNodes</code>.
   */
  def inputConnections( node: SentinelNode,
		        selectedNodes: Set[ SentinelNode ] ): Set[ String ] = 
    Set() ++ node.inputNames
                 .filter( isPotentialParameter( node, _, selectedNodes ) )
  
  /**
   * Create a data structure describing the inputs to the function.
   * This will map nodes to their unconnected inputs.
   * @param nodes The nodes that have been selected.  Any nodes
   * that are not selected are essentially ignored
   * @return a mapping of nodes to their unconnected inputs
   */
  def inputConnections( nodes: Set[ SentinelNode ] ): 
  Map[ SentinelNode, Set[ String ] ] = {
    Map() ++ nodes.map( node => (node, inputConnections( node, nodes )) )
  }
    
  /**
   * Validates that all the given input nodes are accessable via the
   * output node.  Accessible means that they are either ancestors or the node
   * itself.
   * @param inputNodes The input nodes
   * @param outputNode The output node
   * @throws InputNotConnectedException If not all nodes are ancestors
   */
  def validateAccessible( inputNodes: Set[ SentinelNode ],
			  outputNode: SentinelNode ) {
    val accessible = outputNode.ancestors + outputNode
    if ( !inputNodes.forall( accessible.contains( _ ) ) ) {
      throw new InputNotConnectedException( "Input not connected to output" )
    }
  }
}

/**
 * Exception thrown when all given inputs are not ancestors of the given
 * output.
 * @param message A message to show the user
 * @author Kyle Dewey
 */
case class InputNotConnectedException( message: String ) 
     extends Exception( message ) {}

/**
 * Exception thrown when there are multiple possible outputs
 * for a function, or there are no possible outputs.
 * @param message A message to show the user
 * @param numOutputs The number of possible outputs found
 * @author Kyle Dewey
 */
case class BadOutputException
( message: String, val numOutputs: Int ) extends Exception( message ) {
  /**
   * Creates a new exception, using only the number of outputs.
   * A message is generated from this.
   * @param numOutputs The number of possible outputs found
   */
  def this( numOutputs: Int ) =
    this( "Found " + numOutputs + " possible outputs.  Functions must have " +
	  "exactly one output.",
	  numOutputs )
}       

/**
 * Exception thrown when an attempt is made to make an instance factory
 * when there is insufficient information with which to make it.
 * @param message A message to show the user
 * @author Kyle Dewey
 */
case class InsufficientParseTreeInformationException( message: String )
     extends Exception( message ) {}

/**
 * Exception thrown when an attempt is made to manipulate a parameter
 * that we are not aware of.
 * @param message A message to show the user
 * @author Kyle Dewey
 */
case class UnknownParameterNameException( message: String )
     extends Exception( message ) {}

/**
 * Exception thrown when an attempt is made to change the name of
 * an input to a name that is already used.
 * @param message A message to show the user
 * @author Kyle Dewey
 */
case class ParameterNameRepeatException( message: String )
     extends Exception( message ) {}

/**
 * Exception thrown when an attempt is made to make a function
 * with a name that is already registered.
 * @param message A message to show the user
 * @author Kyle Dewey
 */
case class FunctionNameRepeatException( message: String )
     extends Exception( message ) {}

/**
 * Exception thrown when the nodes in the set of selected nodes
 * are at all different from the specified order of nodes.
 * @param message A message to show the user
 * @author Kyle Dewey
 */
case class NodeMismatchException( message: String )
     extends Exception( message ) {}

/**
 * Represents a "prefunction".
 * Prefunctions know what code that they contain and hold reasonable
 * defaults for input names and types, but they do not yet exist in a form
 * that can execute code.  In this form, the user can manipulate certain
 * properties of functions, such as names, descriptions, etc. before actually
 * creating the function.
 * @param inputConnections Nodes that contain unconnected inputs to the
 * function.  For each node, there are names of the unconnected inputs.
 * @param selectedNodes All nodes that are in the function
 * @param inputOrder How to order nodes.  These are the same nodes as seen
 * in selectedNodes
 * @param outputNode The output node for the function
 * @throws NodeMismatchException If the nodes in selectedNodes at all differ
 * from the order of nodes
 * @throws InputNotConnectedException If there are connections missing between
 * <code>inputNodes</code> and <code>outputNode</code>, such that not all inputs
 * are ancestors of the output node.
 * @author Kyle Dewey
 */
class PreFunction
( val inputConnections: Map[ SentinelNode, Set[ String ] ],
  val selectedNodes: Set[ SentinelNode ],
  inputOrder: Seq[ SentinelNode ],
  val outputNode: SentinelNode ) {

  import PreFunctionParam._
  import Function._
  
  // validate input parameters
  validateSame( inputOrder, 
	        selectedNodes )
  validateAccessible( inputNodes, 
		      outputNode )

  // begin instance variables
  var name: Option[ String ] = None
  var description: Option[ String ] = None
  private var _validParams = 
    makeValidParams( inputConnections,
		     inputOrder ).toArray
  private var validParamsByName = paramsMap( validParams )
  // end instance variables

  /**
   * Creates a new PreFunction, given the selected nodes and output
   * node.
   * @param selectedNodes The nodes that have been selected
   * @param inputOrder The order of the selected nodes
   * @param outputNode The output node to use
   */
  def this( selectedNodes: Set[ SentinelNode ], 
	    inputOrder: Seq[ SentinelNode ],
	    outputNode: SentinelNode ) =
    this( Function.inputConnections( selectedNodes ),
	  selectedNodes,
	  inputOrder,
	  outputNode )

  /**
   * Creates a new PreFunction, given the selected nodes.
   * @param selectedNodes The nodes that have been selected
   * @param inputOrder The order of the selected nodes
   * @throws BadOutputException If there are multiple
   * outputs that could be used.  A function is permitted only to
   * have exactly one.
   */
  def this( selectedNodes: Set[ SentinelNode ], 
	    inputOrder: Seq[ SentinelNode ] ) =
    this( selectedNodes,
	  inputOrder,
	  Function.getTailNode( selectedNodes ) )

  /**
   * Gets the valid parameters
   * @return The valid parameters
   */
  def validParams() =
    _validParams

  /**
   * Validates that the given parameter names are the same as those
   * we already have
   * @param paramNames The names of the parameters
   * @throws UnknownParameterNameException If the given parameter
   * name isn't recognized, or if a parameter is missing
   */
  def validateParamsSame( paramNames: Seq[ String ] ) {
    paramNames.foreach( validateParameterName( _ ) )
    val missing = 
      ( Set() ++ validParamsByName.keySet ) -- ( Set() ++ paramNames )
    if ( !missing.isEmpty ) {
      throw new UnknownParameterNameException( "Missing the following params: " +
					       missing.toList
					              .sortWith( _ < _ )
					              .mkString( ", " ) )
    }
  }

  /**
   * Gets the original param info object correlating to the parameter
   * with the given name.
   * @param name The current name of the parameter
   * @return The original param info object correlating to the parameter
   */
  def originalParamInfo( name: String ) =
    validParamsByName( name ).originalParamInfo

  /**
   * Gets the param info object correlating to the
   * parameter with the given name.
   * @param name The current name of the parameter
   * @return The current param info object for the parameter
   */
  def paramInfo( name: String ) =
    validParamsByName( name ).currentParamInfo 

  /**
   * Validates that the given input name is recognized.
   * @param name The name of the input
   * @throws UnknownParameterNameException If the given parameter
   * name isn't recognized
   */
  def validateParameterName( name: String ) {
    if ( !validParamsByName.contains( name ) ) {
      throw new UnknownParameterNameException( "Unknown parameter with name: " + name )
    }
  }

  /**
   * Validates that the given parameter name change can occur
   * @param name The name of the parameter
   * @param newName The new name of the parameter
   * @throws ParameterNameRepeatException If an attempt is made to
   * change to an existing name
   * @throws UnknownParameterNameException If the given parameter
   * name isn't recognized
   */
  def validateChangeParameterName( name: String, newName: String ) {
    validateParameterName( name )
    if ( validParamsByName.contains( newName ) ) {
      throw new ParameterNameRepeatException( "Attempt to change parameter with name " +
					      name + " to " + newName + ", which is already " +
					      "a parameter." )
    }
  }

  /**
   * Changes something on a parameter
   * @param name The name of the parameter
   * @param change Function that can perform the change
   * @throws UnknownParameterNameException If the given parameter
   * name isn't recognized
   */
  def changeParameter( name: String, 
		       change: PreFunctionParam => Unit ) {
    validateParameterName( name )
    
    val param = validParamsByName( name )
    val oldName = param.currentName
    change( param )
    val newName = param.currentName
    if ( oldName ne newName ) {
      validParamsByName -= oldName
      validParamsByName += (newName -> param)
    }
  }

  /**
   * Changes something on a parameter.
   * Note that this assumes that the change function
   * will perform any validation
   * @param name The name of the parameter
   * @param change Function that can perform the change
   * @throws UnknownParameterNameException If the given parameter
   * name isn't recognized
   */
  def changeParamInfo( name: String, 
		       change: ParamInfo => ParamInfo ) {
    changeParameter( name,
		     ( param: PreFunctionParam ) => 
		       param.currentParamInfo = change( param.currentParamInfo ) )
  }

  /**
   * Changes the name of a parameter.
   * @param name The name of the parameter
   * @param newName The new name of the parameter
   * @throws ParameterNameRepeatException If an attempt is made to
   * change to an existing name
   * @throws UnknownParameterNameException If the given parameter
   * name isn't recognized
   */
  def changeParameterName( name: String, newName: String ) {
    validateChangeParameterName( name, newName )

    changeParamInfo( name,
		     _.newName( newName ) )
  }

  /**
   * Changes the type of a parameter.
   * @param name The name of the parameter
   * @param newType The new type of the parameter
   * @throws UnknownParameterNameException If the given parameter
   * name isn't recognized
   * @throws ParameterTypeChangeException If the type change is invalid
   */
  def changeParameterType( name: String, newType: ParamType ) {
    changeParamInfo( name,
		     _.newType( newType ) )
  }

  /**
   * Changes the description of a parameter
   * @param name The name of the parameter
   * @param newDesc The new description of the parameter
   * @throws UnknownParameterNameException If the given parameter
   * name isn't recognized
   */
  def changeParameterDescription( name: String, newDesc: String ) {
    changeParamInfo( name,
		     _.newDesc( newDesc ) )
  }

  /**
   * Changes whether or not the parameter is an array.
   * @param name The name of the parameter
   * @param newIsArray The new isArray value
   * @throws UnknownParameterNameException If the given parameter
   * name isn't recognized
   * @throws ParameterArrayChangeException If the array change is invalid
   */
  def changeParameterIsArray( name: String, newIsArray: Boolean ) {
    changeParamInfo( name,
		     _.newIsArray( newIsArray ) )
  }

  /**
   * Changes whether or not the parameter is required.
   * @param name The name of the parameter
   * @param newIsRequired The new isRequired value
   * @throws UnknownParameterNameException If the given parameter
   * name isn't recognized
   * @throws ParameterRequiredException If the requirement change
   * is invalid
   */
  def changeParameterIsRequired( name: String, newIsRequired: Boolean ) {
    changeParamInfo( name,
		     _.newIsRequired( newIsRequired ) )
  }

  /**
   * Changes whether or not a parameter is used.
   * @param name The name of the parameter
   * @param newIsUsed The new value for whether or not it is used
   * @throws UnknownParameterNameException If the given parameter
   * name isn't recognized
   * @throws ParameterUsedException If the parameter isn't optional and
   * we don't want to use it
   */
  def changeParameterIsUsed( name: String, newIsUsed: Boolean ) {
    changeParameter( name,
		     _.currentIsUsed = newIsUsed )
  }

  /**
   * Changes the order of one or more parameters.
   * @param start The starting row of parameters (inclusive)
   * @param end The ending row of parameters (inclusive)
   * @param to Where to put the range of parameters.  After the move,
   * <code>start</code> will be at the index <code>to</code>.
   */
  def moveParams( start: Int,
		  end: Int,
		  to: Int ) {
    _validParams = SentinelHelpers.moveSubsequence( start,
						    end,
						    to,
						    validParams ).toArray
  }
    
  /**
   * Changes the position of a parameter.
   * @param paramNum The number of the parameter to move
   * @param to Where to move it to.
   */
  def moveParam( paramNum: Int,
		 to: Int ) {
    moveParams( paramNum,
	        paramNum,
	        to )
  }

  /**
   * Gets the names of all inputs
   * @return The names of all inputs.
   */
  def inputNames() =
    Set() ++ validParamsByName.keySet

  /**
   * Gets nodes that have inputs.
   * @return all nodes with inputs
   */
  def inputNodes() =
    Set() ++ inputConnections.filter( !_._2.isEmpty ).keys

  /**
   * Gets the optional inputs for the given input node that
   * are used.
   * @pre The node is an input node
   * @param node The node
   * @return The optional inputs that are used for the node
   */
  def optionalInputsUsed( node: SentinelNode ) = 
      optionalInputs( node ).filter( validParamsByName( _ ).currentIsUsed )
  
  /**
   * Gets the optional inputs for the given input node.
   * @pre The node is an input node
   * @param node The node
   * @return The optional inputs for the node
   */
  def optionalInputs( node: SentinelNode ) =
    Set() ++ validParams.filter( param => 
			  ( param.node.eq( node ) &&
			    !param.currentIsRequired ) )
                        .map( _.currentName )

  /**
   * Given an input node, it gets the names of all connections that
   * must be made to it.
   * @pre The node is an input node
   * @param node The input node
   * @return The names of all inputs that must be connected to it
   * to make it valid
   */
  def inputsNeeded( node: SentinelNode ) = {
    val allInputs =
      Set() ++ inputConnections( node ).map( currentName( node, _ ) )
    ( allInputs -- optionalInputs( node ) ) ++ optionalInputsUsed( node )
  }


  /**
   * Given a node and its current name, it will return its original name.
   * @param node The node
   * @param currentName The current name of the node
   * @return The original name of the node
   */
  def originalName( node: SentinelNode, currentName: String ) = 
    validParams.filter( param => 
      ( param.node.eq( node ) &&
        param.currentName == currentName ) )
               .toList.head.originalName

  /**
   * Given a node and its original name, it will return
   * the current name.
   * @param node The node
   * @param originalName The original name of the node
   * @return The current name of the node
   */
  def currentName( node: SentinelNode, originalName: String ) = 
    validParams.filter( param =>
      ( param.node.eq( node ) &&
       param.originalName == originalName ) )
               .toList.head.currentName

  /**
   * Adds variable nodes to the given input node.
   * These variable nodes correlate to inputs that must be
   * given.
   * @pre The given node is an inputConnection
   * @param asSentinel The node as a sentinel node
   * @param asParse The node as a parse Node
   */
  def addVariableChildren( asSentinel: SentinelNode,
			   asParse: ParseNode ) {
    inputsNeeded( asSentinel ).foreach( input => {
      asParse.attach( new VariableNode( originalName( asSentinel, input ),
				        input ) )
    } )
  }

  /**
   * Makes a usable parse tree, starting at the given node.
   * The given node is assumed to not be a debugging node.
   * @param node The root node of the tree
   * @param name The name of the node
   * @return A parse tree with this node as the root
   */
  def parseTreeNormalNode( node: SentinelNode, name: String ): ParseNode = {
    val asParse = node.parseTreeNode( name )

    // handle internal nodes
    node.inputConnections.keys.foreach( inputName => {
      node.inputConnections( inputName ).foreach( connectedTo => {
	val asSent = connectedTo.asInstanceOf[ SentinelNode ]
	if ( selectedNodes.contains( asSent ) ) {
	  asParse.attach( parseTree( asSent, inputName ) )
	}
      } )
    } )

    // handle inputs
    if ( inputConnections.contains( node ) ) {
      addVariableChildren( node, asParse )
    }

    asParse
  }

  /**
   * Like <code>parseTreeNormalNode</code>, though it is specialized for
   * debugging nodes.  If a debugging node isn't used as a parameter, and
   * it has nothing connected to its input, then it is treated as a constant.
   * @param node The node
   * @param name The name of the node
   * @return A parse tree with this node as the root.
   */
  def parseTreeDebuggingNode( node: SentinelDebuggingNode, name: String ): ParseNode = {
    if ( !hasUsedParameter( node ) &&
	 node.numConnectedInputs == 0 ) {
      new TerminalNode( name, Constant( node.variable.variable ) )
    } else {
      parseTreeNormalNode( node, name )
    }
  }

  /**
   * Determines if the given node has inputs that are used.
   * @param node The node to check
   * @return true if the node is recognized and it has an input that
   * is used.  Else false.
   */
  def hasUsedParameter( node: SentinelNode ) =
    validParams.filter( _.node eq node )
	       .exists( _.currentIsUsed )
    
  /**
   * Makes a usable parse tree, starting at the given node.
   * @pre The given node is within this function
   * @param node The root node of the tree
   * @param name The name of the node
   * @return A parse tree with this base
   */
  def parseTree( node: SentinelNode, name: String ): ParseNode = 
    if ( node.isInstanceOf[ SentinelDebuggingNode ] ) {
      parseTreeDebuggingNode( node.asInstanceOf[ SentinelDebuggingNode ],
			      name )
    } else {
      parseTreeNormalNode( node, name )
    }
  
  /**
   * Makes a usable parse tree from this structure.
   */
  def parseTree(): ParseNode = 
    parseTree( outputNode, "" )

  /**
   * Creates the needed parameters for this function.
   * Note that they are returned in the proper order.
   * @return The needed parameters for this function
   */
  def neededParams() =
    validParams.filter( _.currentIsUsed ).map( _.currentParamInfo )

  /**
   * Creates a mapping appropriate for the <code>neededParams</code>
   * parameter of <code>ParseTreeFactory</code>
   * @return An approproate mapping as described above
   */
  def neededParamsMapping() =
    Map() ++ neededParams.map( param => 
      (param.name, param) )
  
  /**
   * Gets the order in which parameters should be returned.
   */
  def paramOrder() =
    neededParams.map( _.name ).toSeq

  /**
   * Iterates over all the pre function params in the order specified
   * by paramOrder.
   * @param function A function to apply to each pre function param
   * @return A parallel seq of the results
   */
  def mapPreFunctionParams[ T ]( function: PreFunctionParam => T ) =
    validParams.map( function( _ ) )

  /**
   * Iterates over all current param infos in the order specified by
   * paramOrder.
   * @param function A function to apply to each param info object
   * @return A parallel seq of the results
   */
  def mapParamInfos[ T ]( function: ParamInfo => T ) = 
    mapPreFunctionParams( ( p: PreFunctionParam ) =>
      function( p.currentParamInfo ) )

  /**
   * Determines if an instance factory can be created given the
   * current data.
   * @return None if it can, else a Some holding the exception that
   * would be thrown if an attempt were made to create the factory.
   */
  def canMakeInstanceFactory() =
    if ( name.isEmpty ) {
      Some( new InsufficientParseTreeInformationException( "Functions require names." ) )
    } else if ( functionNameRepeat ) {
      Some( new InsufficientParseTreeInformationException( "Function with the given name already exists." ) )
    } else if ( description.isEmpty ) {
      Some( new InsufficientParseTreeInformationException( "Functions require descriptions." ) )
    } else if ( !outputNode.factory.isInstanceOf[ MatcherFactory ] &&
	        !outputNode.factory.isInstanceOf[ ReplacerFactory ] ) {
      Some( new InsufficientParseTreeInformationException( "Unknown kind of " +
							   "instance: " +
							   ParamType.toString( outputNode.factory
									                 .instanceType ) ) )
    } else {
      None
    }

  /**
   * Validates that an instance factory can be created.
   * This simply calls <code>canMakeInstanceFactory</code> and
   * throws the exception if one was generated.
   * @throws InsufficientParseTreeInformationException If there isn't
   * enough information to make a tree with
   */
  def validateCanMakeInstanceFactory() {
    val exception = canMakeInstanceFactory
    if ( exception.isDefined ) {
      throw exception.get
    }
  }

  /**
   * Gets the type of instance that this would create.
   * @return The instance type of this function
   */
  def instanceType() =
    outputNode.factory.instanceType

  /**
   * Determines if the function name is a repeat
   * @return true if the function name is a repeat, else false.
   * Note that if the function name isn't defined, then this
   * returns false
   */
  def functionNameRepeat() =
    name match {
      case Some( n ) => {
	val manager = outputNode.factory match {
	  case m: MatcherFactory => MatcherFactoryManager
	  case r: ReplacerFactory => ReplacerFactoryManager
	  case _ => null
	}
	manager.isRegistered( n )
      }
      case None => false
    }
      
  /**
   * Attempts to make a new instance factory from this function.
   * @throws InsufficientParseTreeInformationException If there isn't
   * enough information to make a tree with
   */
  def instanceFactory() = {
    validateCanMakeInstanceFactory()

    outputNode.factory match {
      case m: MatcherFactory => 
	new ParseTreeMatcherFactory( name.get,
				     description.get,
				     neededParamsMapping,
				     paramOrder,
				     parseTree )
      case r: ReplacerFactory =>
	new ParseTreeReplacerFactory( name.get,
				      description.get,
				      neededParamsMapping,
				      paramOrder,
				      parseTree )
      case _ => 
	throw new InsufficientParseTreeInformationException( "Bad type and validation failed." )
    }
  }
}

      
