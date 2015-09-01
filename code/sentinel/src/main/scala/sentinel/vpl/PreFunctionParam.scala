/*
 * PreFunctionParam.scala
 *
 * Version:
 *     $Id: PreFunctionParam.scala,v 1.2 2011/06/20 22:19:56 kyledewey Exp $
 *
 * Revisions:
 *      $Log: PreFunctionParam.scala,v $
 *      Revision 1.2  2011/06/20 22:19:56  kyledewey
 *      Weakened validation, and added comment regarding
 *      why validation cannot be performed in the particular case.
 *
 *      Revision 1.1  2011/06/19 20:07:40  kyledewey
 *      Initial revision
 *
 *
 */

package sentinel.vpl

import sentinel.model._
import ParamType._

/**
 * Exception thrown when an attept is made to change whether or
 * not a parameter is an array, and the change is invalid.
 * We can always go from array to not array, but never from
 * not array to array.
 * @param message A message to show the user
 * @author Kyle Dewey
 */
case class ParameterArrayChangeException( message: String ) 
     extends Exception( message ) {}

/**
 * Exception thrown when an attempt is made to change
 * whether or not a parameter is required.
 * We can always go from non required to required, but never
 * from required to not required.
 * @param message A message to show the user
 * @author Kyle Dewey
 */
case class ParameterRequiredException( message: String )
     extends Exception( message ) {}

/**
 * Exception thrown when an attempt is made to not use
 * a parameter.  Optional parameters can be discarded in the
 * creation of a function, but not required parameters.
 * @param message A message to show the user
 * @author Kyle Dewey
 */
case class ParameterUsedException( message: String )
     extends Exception( message ) {}

/**
 * Holds helper routines for PreFunctionParams.
 * @author Kyle Dewey
 */
object PreFunctionParam {
  import SentinelHelpers._

  /**
   * Makes a series of valid params given input connections and
   * the order they should go in.  "Valid" means that there won't be
   * name conflicts.
   * @param inputConnections The input connections
   * @param order The order params should go in
   * @return An ordered series of parameters with distinct names
   */
  def makeValidParams( inputConnections: Map[ SentinelNode, Set[ String ] ],
		       order: Seq[ SentinelNode ] ) = {
    val retval = applyOrder( makeParams( inputConnections ).toSeq,
			     order )
    renameRepeats( retval )
    retval
  }
    
  /**
   * Creates PreFunctionParams correlating to the given
   * input connections.  Note that it will not rename repeats.
   * @param inputConnections The input connections.  A mapping of nodes
   * to their unconnected inputs
   * @param inputOrder The order of input nodes.
   * @return PreFunctionParam objects correlating to the given information
   */
  def makeParams( inputConnections: Map[ SentinelNode, Set[ String ] ] ) = 
    inputConnections.keys
                    .flatMap( node => 
		      asParam( node, inputConnections( node ) ) )
  /**
   * Creates a mapping of names of parameters to PreFunctionParam objects.
   * @pre All parameter names are unique
   * @param params The parameters
   * @return A mapping of parameter names to parameters themselves
   */
  def paramsMap( params: Seq[ PreFunctionParam ] ) =
    Map() ++ params.map( param => 
      (param.currentName, param) )

  /**
   * Maps nodes to the PreFunctionParams.
   * Multiple params can be associated with the same node
   * @param params The PreFunctionParams
   * @return A mapping of nodes to the parameters each node has
   */
  def mapByNode( params: Seq[ PreFunctionParam ] ) = 
    multiMap( params.map( param => 
      (param.node, param) ) )

  /**
   * Applies order to the given list of params
   * @param params The unordered parameters
   * @param order The order that parameters should be put in
   * @return The same parameters in the correct order
   */
  def applyOrder( params: Seq[ PreFunctionParam ],
		  order: Seq[ SentinelNode ] ) = {
    val byNode = mapByNode( params )
    order.filter( byNode.contains( _ ) )
         .flatMap( byNode( _ ) )
  }

  /**
   * Gets all the repeated names in the given sequence.
   * @param preFuncs The pre function objects to get the names of.  Note
   * that the names are extracted from <code>paramInfo.name</code>
   * @return A set of repeated names
   */
  def repeatedNames( preFuncs: Seq[ PreFunctionParam ] ) =
    Set() ++ numTimesSeen( 
      preFuncs.map( _.currentName ) ).filter( _._2 > 1 ).keys

  /**
   * Renames preFuncs so that names do not collide.
   * If a name doesn't collide, then it won't be changed.  Note the
   * name refers to the name on the paramInfo object.
   * Note that this is destructive.
   * @param preFuncs The preFuncts to rename
   */
  def renameRepeats( preFuncs: Seq[ PreFunctionParam ] ) = {
    var repeats = 
      Map() ++ repeatedNames( preFuncs ).map( x => (x, 1) )

    // gets the new name to use
    def getNewName( oldName: String ) = 
      if ( repeats.contains( oldName ) ) {
	val num = repeats( oldName )
	val newName = oldName + num
	repeats += (oldName -> (num + 1))
	newName
      } else {
	oldName 
      }
    
    preFuncs.foreach( preFunc => {
      val oldName = preFunc.currentName
      val newName = getNewName( oldName )
      if ( oldName ne newName ) {
	preFunc.currentParamInfo = preFunc.currentParamInfo.newName( newName )
      }
    })
    preFuncs
  }

  /**
   * Makes a PreFunctionParam object based on the given node and input.
   * @param node The node
   * @param input The input on the node
   * @return A PreFunctionParam object correlating to the information
   */
  def asParam( node: SentinelNode, input: String ): PreFunctionParam = {
    val paramInfo = node.factory.validParams( input )
    new PreFunctionParam( paramInfo,
			  paramInfo.isRequired,
			  node )
  }

  /**
   * Makes PreFunctionParam objects based on the given node
   * @param node The node
   * @param inputs Set of unconnected inputs for the node
   * @return PreFunctionParam objects for the node, in the order that
   * the node specifies
   */
  def asParam( node: SentinelNode, 
	       inputs: Set[ String ] ): Seq[ PreFunctionParam ] = {
    node.factory
        .paramOrder
        .filter( inputs.contains( _ ) )
        .map( asParam( node, _ ) )
  }
}

import sentinel.utils.interactive.TypeConversionValidator

/**
 * Dictates the kinds of conversions that are possible.
 * @author Kyle Dewey
 */
object ConversionValidator extends TypeConversionValidator {
  // given the original value, it returns the possible values
  // that it can become
  val convertableIsArray = 
    Map( true -> Set( true, false ),
	 false -> Set( false ) )
  val convertableIsRequired =
    Map( true -> Set( true ),
	 false -> Set( true, false ) )

  // given if it's required, it returns the possible
  // values that it can become
  val convertableIsUsed =
    convertableIsRequired
  
  /**
   * Validates that the given parameter array change is valid.
   * We can always go from array -> not array, but never the opposite.
   * @param oldIsArray the old isArray value
   * @param newIsArray the new isArray value
   * @throws ParameterArrayChangeException If the array change is invalid
   */
  def validateChangeParameterIsArray( oldIsArray: Boolean, newIsArray: Boolean ) {
    validateContains( oldIsArray,
		      newIsArray,
		      convertableIsArray,
		      new ParameterArrayChangeException( 
			"Cannot go from not being an array to being an array." ) )
  }

  /**
   * Validates that the given requirement change is valid.
   * We can always go from not being required to being required,
   * but never the opposite.
   * @param oldIsRequired The old isRequired value
   * @param newIsRequired The new isRequired value
   * @throws ParameterRequiredException If the requirement change
   * is invalid
   */
  def validateChangeParameterIsRequired( oldIsRequired: Boolean, newIsRequired: Boolean ) {
    validateContains( oldIsRequired,
		      newIsRequired,
		      convertableIsRequired,
		      new ParameterRequiredException( 
			"Cannot go from being required to not being required." ) )
  }
  
  /**
   * Validates that the given use change is valid.
   * Optional parameters can be discarded and not used, but only
   * optional parameters.
   * @param oldIsUsed Whether or not it used to be used
   * @param newIsUsed If we want to use this now
   * @throws ParameterUsedException If the parameter isn't optional and
   * we don't want to use it
   */
  def validateChangeParameterIsUsed( oldIsUsed: Boolean, newIsUsed: Boolean ) {
    validateContains( oldIsUsed,
		      newIsUsed,
		      convertableIsUsed,
		      new ParameterUsedException( 
			"Cannot discard a required parameter." ) )
  }
}

/**
 * Simple container class representing parameters.
 * We need to store both the original information of the parameter and
 * current information of the parameter, in order to validate conversions.
 * @param _currentParamInfo The current param info object
 * @param originalParamInfo The original param info object
 * @param currentIsUsed The current isUsed value
 * @param originalIsUsed The original value for isUsed
 * @param node The node that this is associated with
 * @author Kyle Dewey
 */
class PreFunctionParam( private var _currentParamInfo: ParamInfo,
		        val originalParamInfo: ParamInfo,
		        private var _currentIsUsed: Boolean,
		        val originalIsUsed: Boolean,
		        val node: SentinelNode ) {
  /**
   * Creates a PreFunctionParam from the given original information.
   * The original and current values are set to be the same
   * @param originalParamInfo The original param info
   * @param originalIsUsed The original isUsed value
   * @param node The node that we are associated with
   */
  def this( originalParamInfo: ParamInfo,
	    originalIsUsed: Boolean,
	    node: SentinelNode ) = {
    this( originalParamInfo,
	  originalParamInfo,
	  originalIsUsed,
	  originalIsUsed,
	  node )
  }

  /**
   * Gets the current isUsed value.
   * @return The current isUsed value
   */
  def currentIsUsed =
    _currentIsUsed

  /**
   * Sets the current isUsed value.
   * @param newIsUsed The new isUsed value
   * @throws ParameterUsedException If the parameter isn't optional and
   * we don't want to use it
   */
  def currentIsUsed_=( newIsUsed: Boolean ) {
    validateChangeParameterIsUsed( newIsUsed )
    _currentIsUsed = newIsUsed
  }

  /**
   * Gets the current param info object.
   * @return The current param info object
   */
  def currentParamInfo = 
    _currentParamInfo

  /**
   * Sets the current param info object.
   * @param newParamInfo The new current param info object
   * @throws ParameterTypeChangeException If the type change is invalid
   * @throws ParameterArrayChangeException If the array change is invalid
   * @throws ParameterRequiredException If the requirement change is invalid
   */
  def currentParamInfo_=( newParamInfo: ParamInfo ) {
    validateChangeParamInfo( newParamInfo )
    _currentParamInfo = newParamInfo
  }

  /**
   * Gets the original name.
   * @return The original name
   */
  def originalName() =
    originalParamInfo.name

  /**
   * Gets the current name
   * @return The current name
   */
  def currentName() =
    currentParamInfo.name

  /**
   * Gets the current description.
   * @return the current description
   */
  def currentDesc() =
    currentParamInfo.desc

  /**
   * Gets the original description
   * @return The original description
   */
  def originalDesc() =
    originalParamInfo.desc

  /**
   * Gets the current type
   * @return The current type
   */
  def currentType() =
    currentParamInfo.paramType

  /**
   * Gets the current isArray value
   * @return The current isArray value
   */
  def currentIsArray() =
    currentParamInfo.isArray

  /**
   * Gets the current isRequired value
   * @return The current isRequired value
   */
  def currentIsRequired() =
    currentParamInfo.isRequired

  /**
   * Gets the original type
   * @return The original type
   */
  def originalType() =
    originalParamInfo.paramType

  /**
   * Gets the original isArray value.
   * @return The original isArray value
   */
  def originalIsArray() =
    originalParamInfo.isArray

  /**
   * Gets the original isRequired value
   * @return The original isRequired value
   */
  def originalIsRequired() =
    originalParamInfo.isRequired

  /**
   * Compares the current names.
   * @param other The other param
   * @return true if the current names are different, else false
   */
  def currentNameDifferent( other: PreFunctionParam ) =
    currentName != other.currentName

  /**
   * Compares the original names.
   * @param other The other param
   * @return true if the original names are different, else false
   */
  def originalNameDifferent( other: PreFunctionParam ) =
    originalName != other.originalName

  /**
   * Validates everything about the given ParamInfo object, as if we were
   * changing everything in ourselves to it.  Assumes that the name
   * change is valid, which requires knowledge of other PreFunctionParam
   * objects to verify properly.
   * @param paramInfo The param info object to verify all fields of
   * @throws ParameterTypeChangeException If the type change is invalid
   * @throws ParameterArrayChangeException If the array change is invalid
   * @throws ParameterRequiredException If the requirement change is invalid
   */
  def validateChangeParamInfo( paramInfo: ParamInfo ) {
    validateChangeParameterType( paramInfo.paramType )
    validateChangeParameterIsArray( paramInfo.isArray )
    validateChangeParameterIsRequired( paramInfo.isRequired )
  }

  /**
   * Validates that the given parameter type change is valid.
   * @param newType The new type
   * @throws ParameterTypeChangeException If the type change is invalid
   */
  def validateChangeParameterType( newType: ParamType ) {
    ConversionValidator.validateChangeParameterType( originalType, 
						     newType )
  }

  /**
   * Validates that the given isArray change is valid.
   * @param newIsArray The new isArray value
   * @throws ParameterArrayChangeException If the array change is invalid
   */
  def validateChangeParameterIsArray( newIsArray: Boolean ) {
    ConversionValidator.validateChangeParameterIsArray( originalIsArray, 
						        newIsArray )
  }

  /**
   * Validates that the given isRequired change is valid
   * @param newIsRequired The new isRequired value
   * @throws ParameterRequiredException If the requirement change
   * is invalid
   */
  def validateChangeParameterIsRequired( newIsRequired: Boolean ) {
    ConversionValidator.validateChangeParameterIsRequired( originalIsRequired,
							   newIsRequired )
    // this currently can't be done since Swing's model forces programmatic
    // cascades to occur after the initial change has already been performed
    /*if ( newIsRequired && !currentIsUsed ) {
      throw new ParameterRequiredException( 
	"Required parameters must be used." )
    }*/
  }

  /**
   * Validates that the given use change is valid.
   * @param newIsUsed The new value for isUsed
   * @throws ParameterUsedException If the parameter isn't optional and
   * we don't want to use it
   */
  def validateChangeParameterIsUsed( newIsUsed: Boolean ) {
    ConversionValidator.validateChangeParameterIsUsed( originalIsUsed,
						       newIsUsed )
  }
}

