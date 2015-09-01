/*
 * Factory.scala
 */

package sentinel.model

/**
 * Contains helper methods relevant to InstanceFactory
 * @author Kyle Dewey
 */
object InstanceFactory {
  // begin constants
  val SEPARATOR = ", "
  // end constants

  /**
   * Gets an array of optional params.
   * @param name The name of the param
   * @param params The listing of params
   * @return All params with the given name, or None if it wasn't passed
   */
  def opAsArray( name: String, 
		 params: Seq[ NamedParam ] ): Option[ Array[ Param ] ] = {
    val result = params.filter( _.name == name )
    if ( !result.isEmpty ) {
      Some( result.map( _.param ).toArray )
    } else None
  }

  /**
   * Given a listing of params and the name of a param,
   * gets only those params that match the name.  Assumes that
   * The items are of the given type.
   * @param name The name to look for
   * @param params The listing of params
   * @return All params with that name, or an empty sequence
   *         if there were none.
   */
  def asArray( name: String, params: Seq[ NamedParam ] ): Array[ Param ] =
    opAsArray( name, params ).get

  /**
   * Gets the optional param with the given name.
   * @param name The name of the desired param
   * @param params The listing of params
   * @return The param, or None if it wasn't passed.
   */
  def opParam( name: String, params: Seq[ NamedParam ] ): Option[ Param ] = {
    val asArray = opAsArray( name, params )
    if ( asArray.isDefined ) {
      Some( asArray.get.head )
    } else None
  }

  /**
   * Gets the param with the given name.  Note that if multiple params
   * have the same name, it will return the first one found.
   * @param name The name of the desired param
   * @param params The listing of params
   * @return the first param found
   */
  def param( name: String, params: Seq[ NamedParam ] ): Param =
    opParam( name, params ).get

  /**
   * Converts the given array of parameters to an array of matchers
   * @param params The params
   * @return An array of matchers
   * @throws ValueException If a conversion failed
   */
  def matchers( params: Seq[ Param ] ) =
    params.map( _.matcherValue )

  /**
   * Given a sequence, will sort the items in the sequence and produce
   * a string that's applicable for an error
   * @param items The items in the sequence
   * @return an appropriate string for an error message
   */
  def errStr( items: Seq[ String ] ) =
    items.toList.sortWith( _ < _ ).mkString( SEPARATOR )

  /**
   * Gets information about a class as to whether or not it is
   * a built in.
   * @param theClass The class to get the information of
   * @return whether or not it's a built in, and the name of
   *         the JVM class that backs it if it is
   */
  def builtInInfo( theClass: InstanceFactory[ _ ] ) = {
    val builtIn = theClass.isInstanceOf[ ReflectionFactory[ _ ] ]
    var className: Option[ String ] = None
    if ( builtIn ) {
      className = Some( theClass.asInstanceOf[ ReflectionFactory[ _ ] ]
                                .JVMClassName )
    }
    ( builtIn,
      className )
  }

  /**
   * Converts the given boolean value to "Yes" or "No", depending on
   * whether it is true or false.
   * @param value The boolean value to convert
   * @return "Yes" for true, or "No" for false
   */
  def toYesNo( value: Boolean ) =
    if ( value ) "Yes" else "No"
}

/**
 * <p>Defines a factory that can create objects that can
 * take parameters. Note that within sentinel, there are two acceptable
 * ways to do this.  The first is by creating a class in a JVM-aware
 * language, and then creating instances of that.  The second is
 * by using parse trees to build new classes from existing classes
 * within the framework.</p>
 *
 * @param name The name of the factory
 * @param desc A description of the factory
 * @param neededParams What params are needed to instantiate
 *        an object.  The keys are the names of the params and
 *        the values are detailed information about the params
 * @param paramOrder The order of the parameters
 * 
 * @author Kyle Dewey
 */
abstract class InstanceFactory[ T <: Instance ]
( val name: String,
  val desc: String,
  val validParams: Map[ String, ParamInfo ],
  val paramOrder: Seq[ String ] ) 
extends Ordered[ InstanceFactory[ T ] ] with Describable with SentinelDescription {
  
  import InstanceFactory._
  import ParamType._

  /**
   * Gets a describer for this factory.
   * Note that this simply returns the factory itself
   * @return <code>this</code>
   */
  def describer() =
    this

  /**
   * Gets a description of this factory
   * @return <code>desc</code>
   */
  def description() =
    desc

  /**
   * Gets the name of all inputs, in their desired order
   * @return <code>paramOrder.toArray</code>
   */
  def inputNames() =
    paramOrder.toArray

  /**
   * Maps the valid params against the given function.
   * They are called in order of paramOrder
   * @param function The function to map with
   * @return The mapping result
   */
  def mapParamInfos[ T ]( function: ParamInfo => T ) =
    paramOrder.map( ( paramName: String ) =>
      function( validParams( paramName ) ) )

  /**
   * Validates the given input name.
   * @param input The name of the input to validate
   * @throws InputNameException If the name of the input isn't recognized
   */
  def validateName( input: String ) {
    if ( !validParams.contains( input ) ) {
      throw new UnknownInputException( "Unknown input with name: " + input )
    }
  }

  /**
   * Gets a description of the given input.
   * @param input The name of the input
   * @return A description of the given input
   * @throws UnknownInputException If the given input name isn't
   * recognized
   */
  def inputDescription( input: String ) = {
    validateName( input )
    validParams( input ).desc
  }
  
  /**
   * Gets a detailed description of the given input.
   * @param input The name of the input
   * @return The input name, description, type, whether or not it's an
   * array, and whether or not it's required, in an array
   * @throws UnknownInputException If the given input name isn't recognized
   */
  override def detailedDescription( input: String ) = {
    validateName( input )
    val paramInfo = validParams( input )
    Array( paramInfo.name,
	   paramInfo.desc,
	   ParamType.toString( paramInfo.paramType ),
	   InstanceFactory.toYesNo( paramInfo.isArray ),
	   InstanceFactory.toYesNo( paramInfo.isRequired ) )
  }

  
  /**
   * Compares this factory to another factory.
   * The comparison is based upon the name.
   * @param other The other factory to compare to
   * @return <code>this.name.compare( other.name )</code>
   */
  override def compare( other: InstanceFactory[ T ] ) =
    name.compare( other.name )

  /**
   * Verifies that the given sequence of params is valid.
   * If not, it will throw the appropriate exceptions
   * 
   * @param params The params to verify
   * @return Converted, safe params
   * @throws ParameterNameException If one of the parameters
   *         has a name that we don't recognize
   * @throws ParameterTypeException If the expected types and
   *         actual types of the param differ
   * @throws ParameterRequirementException If a neccessary param
   *         is missing
   * @throws ParameterArrayException If our expectation of an array
   *         or not is different from the given param
   */
  def validateParams( params: Seq[ NamedParam ] ) = {
    validateNames( params )
    validateRequirements( params )
    validateArrays( params )
    validateAndConvertTypes( params )
  }

  /**
   * Verifies that we recognize all the names in the given
   * sequence of params
   * @param params Params to verify
   * @throws ParameterNameException If we do not recognize a name
   *         in the given listing of params
   */
  def validateNames( params: Seq[ NamedParam ] ) = {
    val invalid = params.filter( param =>
      !validParams.contains( param.name ) )
    if ( invalid.size != 0 ) {
      throw new ParameterNameException( "Found unrecognized names: " +
				        errStr( invalid.map( _.name ) ) )
    }
  }

  /**
   * Makes sure that all parameters are of the appropriate type.
   * If they all are, then they will be returned as-is.
   * If conversions had to be made, then a new sequence containing
   * the converstions will be returned.
   * If the types are invalid or a conversion is impossible, then
   * an exception is thrown.
   * @param params The params to verify and/or convert.
   * @throws ParameterTypeException If a type didn't match and conversion
   *         was impossible
   */
  def validateAndConvertTypes( params: Seq[ NamedParam ] ) = 
    params.map( validateAndConvertType( _ ) )

  /**
   * Given a param, it will verify that the type matches.
   * If the type doesn't match, it will attempt a conversion.
   * If the conversion failed, then this throws an exception.
   * @param param The param to verify
   * @return The NamedParameter to use
   * @throws ParameterTypeException If the type didn't match and
   *         conversion was impossible
   */
  def validateAndConvertType( param: NamedParam ): NamedParam = {
    val expectedType = validParams( param.name ).paramType
    var retval = param

    // if the type wasn't expected, then try a conversion
    if ( param.param.getType != expectedType ) {
      val converted = param.param.convertTo( expectedType )
      if ( converted.isEmpty ) {
	throw new ParameterTypeException( param, expectedType )
      } else {
	retval = new NamedParam( param.name,
				 converted.get )
      }
    }

    retval
  }
      
  /**
   * Verifies that we have all required params are accounted for.
   * @param params to verify
   * @throws ParameterRequirementException If a neccessary param
   *         is missing
   */
  def validateRequirements( params: Seq[ NamedParam ] ) = {
    val required = 
      getRequiredNames -- params.map( _.name )
    if ( required.size != 0 )
      throw new ParameterRequirementException( "Missing required parameters: " +
					       errStr( required.toList ) )
  }

  /**
   * Verifies that if an item is expected to be an array,
   * then it is an array, and vice-versa.  Note that arrays
   * are implemented merely by specifying the same parameter
   * name multiple times.
   * @param params Params to verify
   * @throws ParameterArrayException If something is (or is not) found
   *         to be an array and the opposite was expected
   */
  def validateArrays( params: Seq[ NamedParam ] ) = {
    val numSeen = getNumTimesSeen( params )
    val invalid = numSeen.filter( pair =>
      pair._2 > 1 && !validParams( pair._1 ).isArray )
    if ( invalid.size != 0 ) 
      throw new ParameterArrayException( "The following were treated as " +
					 "arrays, but are not: " +
					 errStr( invalid.keys.toList ) )
  }

  /**
   * Gets the number of times each name is observed in the given
   * list of params
   * @param params The params
   * @return a mapping from the name to the number of times it is seen
   */
  def getNumTimesSeen( params: Seq[ NamedParam ] ) =
    SentinelHelpers.numTimesSeen( params.map( _.name ) )
			 
  /**
   * Gets a set of the names of all required params.
   * @return A set of names of all required params
   */
  def getRequiredNames() =
    Set() ++ validParams.filter( _._2.isRequired ).keys
  
  /**
   * Instantiates an object without any parameters.
   * Note that this is equivalent to <code>instantiate( Seq(), optimize )</code>
   * @param optimize If any optimizations should be performed.
   * @return A new object initialized without any parameters
   * @throws ParameterNameException If one of the parameters
   *         has a name that we don't recognize
   * @throws ParameterTypeException If the expected types and
   *         actual types of the param differ
   * @throws ParameterRequirementException If a neccessary param
   *         is missing
   * @throws ParameterArrayException If our expectation of an array
   *         or not is different from the given param
   * @throws ParameterizedInstantiationException If the params were ok
   *         but the object could not otherwise be instantiated
   */
  def instantiate( optimize: Boolean ): T =
    instantiate( Seq(), optimize )

  /**
   * Instantitates an object given a listing of params.
   * @param params The params to instantiate an object with
   * @param optimize Whether or not to perform any optimizations
   * @return A new object initialized with the given params
   * @throws ParameterNameException If one of the parameters
   *         has a name that we don't recognize
   * @throws ParameterTypeException If the expected types and
   *         actual types of the param differ
   * @throws ParameterRequirementException If a neccessary param
   *         is missing
   * @throws ParameterArrayException If our expectation of an array
   *         or not is different from the given param
   * @throws ParameterizedInstantiationException If the params were ok
   *         but the object could not otherwise be instantiated
   */
  def instantiate( params: Seq[ NamedParam ], optimize: Boolean ): T = {
    val newParams = validateParams( params )
    try {
      internalInstantiate( newParams, optimize ).convertTo( instanceType )
                                                .get
                                                .asInstanceOf[ T ]
    } catch {
      case e: ParameterizedInstantiationException => throw e
      case e: Exception => 
	throw new ParameterizedInstantiationException( e )
    }
  }

  /**
   * Gets the names of all parameters in sorted order.  Required parameters
   * go first in sorted order, which are followed by optional parameters
   * in sorted order.
   * @return the names of all parameters in the order decribed above
   */
  def getSortedParamNames(): Seq[ String ] = 
    ParamInfo.sortParams( validParams.values.toList ).map( _.name )
  
  /**
   * Gets the kind of instances that can be created by
   * this factory.
   * @return The kind of instances this factory makes.
   */
  def instanceType(): ParamType

  /**
   * Instantiates an object given a listing of params.
   * Note that the params are guarenteed to be valid.
   * Also note that the params passed to internalInstantiate
   * MAY NOT be the same params passed to instantiate; this
   * can happen when a type conversion had to take place.
   * @param params The params to instantiate an object with
   * @param optimize Whether or not to perform any optimizations
   * @return A new object instantiated with the given params
   * @throws ParameterizedInstantiationException If the params were ok
   *         but the object could not otherwise be instantiated
   */
  protected def internalInstantiate( params: Seq[ NamedParam ], 
				     optimize: Boolean ): T
} // InstanceFactory[ T ]

/**
 * Like InstanceFactory, but what is produced is determined
 * via reflection.
 * @param name The name of the factory.
 * @param desc A description of the factory
 * @param neededParams The params the objects that will be created take
 * @param paramOrder The order of the params
 * @param JVMClassName The name of the JVM class that backs the factory
 * @throws ClassNotFoundException If the given class name doesn't exist
 * @throws NoSuchMethodException If there was no constructor that takes
 *         Seq[ NamedParam ]
 * @author Kyle Dewey
 */
abstract class ReflectionFactory[ T <: Instance ]( name: String,
						   desc: String,
						   neededParams: Map[ String, ParamInfo ],
						   paramOrder: Seq[ String ],
						   val JVMClassName: String  )
extends InstanceFactory[ T ]( name,
			      desc, 
			      neededParams,
			      paramOrder ) {
  private val constructor = 
    Class.forName( JVMClassName )
         .getConstructor( classOf[ String ], classOf[ Seq[ Param ] ] )

  /**
   * Instantiates an object with the given params.
   * @param params The params for the object
   * @param optimize Whether or not to perform any optimizations.  At the
   * moment, this does nothing.
   * @return a new object instantiated with the given params
   * @throws ParameterizedInstantiationException If the params were ok
   *         but the object could not otherwise be instantiated
   */
  def internalInstantiate( params: Seq[ NamedParam ], optimize: Boolean ) = 
    constructor.newInstance( name, params ).asInstanceOf[ T ]
}

/**
 * Represents a node in a parse tree.  Note that a node may
 * or may not be a terminal.
 * @param name The name of the node
 * @author Kyle Dewey
 */
abstract class ParseNode( val name: String ) {
  private var children: List[ ParseNode ] = List()

  /**
   * Attaches a child node to this node.
   * @param node The child node
   */
  def attach( node: ParseNode ) =
    children ::= node

  /**
   * Gets child nodes of this node.
   * @return child nodes of this node
   */
  def getChildren() =
    children.reverse.toSeq

  /**
   * Gets the value of this node.  Assumes that
   * the parse tree has been completely built.
   * Note that this returns a Seq in order to accomodate
   * given parameters which are arrays; under all other
   * conditions, it is guarenteed to return a Seq of
   * size 1.
   * @param params Parameters to the tree
   * @return The evaluated parameter
   */
  def getValue( params: Seq[ NamedParam ] ): Seq[ NamedParam ]

  /**
   * Like <code>getValue</code>, but it performs optimizations.
   * Note that the default is to simply forward the call to
   * getValue().
   * @param params Parameters for the tree
   * @return The evaulated parameter, with optimizations.  The default is
   * <code>getValue( params )</code>
   */
  def getOptimizedValue( params: Seq[ NamedParam ] ) =
    getValue( params )
}

/**
 * An instance returned by a parse tree
 * This is needed since the parameters and class name returned by the
 * root instance will NOT match what was given to the parse tree factory
 * @param className the class name
 * @param params The parameters that were passed to the parse tree
 * @param instance The instance itself
 * @author Kyle Dewey
 */
class ParseTreeInstance[ T <: Instance ]( val className: String,
					  val params: Seq[ NamedParam ],
					  val instance: T ) 
extends Instance with Proxy {
  import ParamType._

  /**
   * Gets the underlying instance
   * @return The underlying instance
   */
  def self() = instance
  
  def proxify( param: Param ): Param =
    if ( param eq instance ) this else param
  def proxify( param: Option[ Param ] ): Option[ Param ] = 
    if ( param.isDefined ) {
      Some( proxify( param.get ) )
    } else {
      None
    }
      
  // begin delegated methods
  override def sentStringValue() = instance.sentStringValue
  override def sentIntValue() = instance.sentIntValue
  override def sentRealValue() = instance.sentRealValue
  override def sentCharValue() = instance.sentCharValue
  override def matcherValue() = instance.matcherValue
  override def replacerValue() = instance.replacerValue
  override def toSentString() = instance.toSentString 
  override def toSentInt() = instance.toSentInt 
  override def toSentReal() = instance.toSentReal 
  override def toSentChar() = instance.toSentChar 
  override def toReplacer() = instance.toReplacer
  override def toMatcher() = instance.toMatcher 
  override def convertTo( toType: ParamType ) = instance.convertTo( toType )
  override def getType() = instance.getType
  override def isInstanceType() = instance.isInstanceType
  override def isConstantType() = instance.isConstantType
  override def isVariableType() = instance.isVariableType
  override def typeName() = instance.typeName
  override def printableValue() = instance.printableValue
  override def isPure() = instance.isPure
  // end delegated methods
}

/**
 * A matcher returned from a parse tree.
 * @param className The name of the class
 * @param params The parameters that were passed to the tree
 * @param matcher The matcher
 * @author Kyle Dewey
 */
class ParseTreeMatcher( className: String,
		    params: Seq[ NamedParam ],
		    matcher: Matcher )
extends ParseTreeInstance[ Matcher ]( className, params, matcher ) with Matcher {
  /**
   * Delegates to the matcher
   * @return The result of delegating to the matcher
   */
  override def matches() = instance.matches
}

/**
 * A replacer returned from a parse tree
 * @param className The name of the class
 * @param params The parameters that were passed to the tree
 * @param replacer The replacer
 * @author Kyle Dewey
 */
class ParseTreeReplacer( className: String, 
			 params: Seq[ NamedParam ],
			 replacer: Replacer )
extends ParseTreeInstance[ Replacer ]( className, params, replacer ) with Replacer {
  /**
   * Delegates to the replacer
   * @return The result of delegating to the replacer
   */
  override def replace() = instance.replace
}

/**
 * A factory that creates objects based upon a parse tree.
 * @param name The name of the factory.
 * @param desc A description of the factory
 * @param neededParams The parameters that are valid for the tree
 * @param paramOrder The order of the params
 * @param tree The parse tree
 * @author Kyle Dewey
 */
abstract class ParseTreeFactory[ T <: Instance ]( name: String,
						  desc: String,
						  neededParams: Map[ String, ParamInfo ],
						  paramOrder: Seq[ String ],
						  val tree: ParseNode ) 
extends InstanceFactory[ T ]( name,
			      desc,
			      neededParams,
			      paramOrder ) {
  /**
   * Creates a new param based on the parse tree
   * @param params Parameters to the underlying instance
   * @param optimize Whether or not to optimize
   * @return A new instance of the underlying object
   * @throws ParameterizedInstantiationException If the params were ok but
   *         the object could not otherwise be instantiated
   */
  def rootParam( params: Seq[ NamedParam ], optimize: Boolean ) = {
    val root =
      if ( optimize ) tree.getOptimizedValue( params )
      else tree.getValue( params )

    if ( root.size != 1 ) {
      throw new ParameterizedInstantiationException( "Tree returned array " +
						     "of params instead of " +
						     "a single param" )
    }
    root.head.param
  }
}

/**
 * Factory that can create matchers.
 * @author Kyle Dewey
 */
trait MatcherFactory extends InstanceFactory[ Matcher ] {
  /**
   * Gets that this factory creates instances of matchers.
   * @return ParamType.MatcherType
   */
  override def instanceType() =
    ParamType.MatcherType
}

/**
 * Factory that can create replacers.
 * @author Kyle Dewey
 */
trait ReplacerFactory extends InstanceFactory[ Replacer ] {
  /**
   * Gets that this factory creates instances of replacers.
   * @return ParamType.ReplacerType
   */
  override def instanceType() =
    ParamType.ReplacerType
}

/**
 * Factory that creates matchers via reflection.
 * @param name The name of the factory
 * @param desc A description of the factory
 * @param validParams The parameters that are needed to initalize objects
 * @param paramOrder The order of the params
 * @param JVMClassName The name of the class that backs instances 
 *        created by the factory
 * @throws ClassNotFoundException If the given class name doesn't exist
 * @throws NoSuchMethodException If there was no constructor that takes
 *         Seq[ NamedParam ] * @author Kyle Dewey
 */
class ReflectionMatcherFactory( name: String,
			        desc: String,
			        validParams: Map[ String, ParamInfo ],
			        paramOrder: Seq[ String ],
			        JVMClassName: String )
extends ReflectionFactory[ Matcher ]( name,
				      desc,
				      validParams,
				      paramOrder,
				      JVMClassName ) with MatcherFactory {}

/**
 * Factory that creates matchers via parse trees.
 * @param name The name of the factory
 * @param desc A description of the factory
 * @param validParams The parameters that are needed to initialize objects
 * @param paramOrder The order of the params
 * @param tree The root of the parse tree
 * @throws ClassNotFoundException If the given class name doesn't exist
 * @throws NoSuchMethodException If there was no constructor that takes
 *         Seq[ NamedParam ]
 * @author Kyle Dewey
 */
class ParseTreeMatcherFactory( name: String,
			       desc: String,
			       validParams: Map[ String, ParamInfo ],
			       paramOrder: Seq[ String ],
			       tree: ParseNode )
extends ParseTreeFactory[ Matcher ]( name,
				     desc,
				     validParams,
				     paramOrder,
				     tree ) with MatcherFactory {
  /**
   * Creates a new matcher based on the parse tree
   * @param params Parameters to the underlying matcher
   * @param optimize Whether or not to optimize
   * @return A new matcher
   * @throws ParameterizedInstantiationException If the params were ok
   * but the object could not otherwise be instantiated
   */
  def internalInstantiate( params: Seq[ NamedParam ], optimize: Boolean ) = {
    val root = rootParam( params, optimize )
    val asMatcher = root.toMatcher
    if ( asMatcher.isDefined &&
         asMatcher.get.isInstanceOf[ Matcher ] ) {
      new ParseTreeMatcher( name, 
			    params, 
			    asMatcher.get.asInstanceOf[ Matcher ] )
    } else {
      throw new ParameterizedInstantiationException( 
	"Parse tree did not return matcher. Returned: " + root.typeName )
    }
  }
}

/**
 * Factory that creates replacers via reflection.
 * @param name The name of the factory
 * @param desc A description of the factory
 * @param validParams The parameters that are needed to initalize objects
 * @param paramOrder The order of the params
 * @param JVMClassName The name of the class that backs instances 
 *        created by the factory
 * @author Kyle Dewey
 */
class ReflectionReplacerFactory( name: String,
			         desc: String,
			         validParams: Map[ String, ParamInfo ],
				 paramOrder: Seq[ String ],
			         JVMClassName: String )
extends ReflectionFactory[ Replacer ]( name,
				       desc,
				       validParams,
				       paramOrder,
				       JVMClassName ) with ReplacerFactory {}

/**
 * Factory that creates replacers via parse trees.
 * @param name The name of the factory
 * @param desc A description of the factory
 * @param validParams The parameters that are needed to initialize objects
 * @param paramOrder The order of the params
 * @param tree The root of the parse tree
 * @author Kyle Dewey
 */
class ParseTreeReplacerFactory( name: String,
			        desc: String,
			        validParams: Map[ String, ParamInfo ],
			        paramOrder: Seq[ String ],
			        tree: ParseNode )
extends ParseTreeFactory[ Replacer ]( name,
				      desc,
				      validParams,
				      paramOrder,
				      tree ) with ReplacerFactory {
  /**
   * Creates a new replacer based on the parse tree
   * @param params Parameters to the underlying replacer
   * @param optimize Whether or not to optimize
   * @return A new replacer
   * @throws ParameterizedInstantiationException If the params were ok
   * but the object could not otherwise be instantiated
   */
  def internalInstantiate( params: Seq[ NamedParam ], optimize: Boolean ) = {
    val root = rootParam( params, optimize )
    val asReplacer = root.toReplacer
    if ( asReplacer.isDefined &&
	 asReplacer.get.isInstanceOf[ Replacer ] ) {
      new ParseTreeReplacer( name, 
			     params, 
			     asReplacer.get.asInstanceOf[ Replacer ] )
    } else {
      throw new ParameterizedInstantiationException( 
	"Parse tree did not return replacer. Returned: " + root.typeName )
    }
  }
}

/**
 * Assists in the creation of matcher factories.
 * @author Kyle Dewey
 */
object MatcherFactory {
  /**
   * Creates a ReflectionMatcherFactory.
   * @param name The name of the factory
   * @param desc A description of the factory
   * @param validParams Parameters for objects created by the factory
   * @param paramOrder The order of the parameters
   * @param className The JVM class name that backs the instance
   * @return A new matcher factory that can create these instances
   */
  def apply( name: String,
	     desc: String,
	     validParams: Map[ String, ParamInfo ],
	     paramOrder: Seq[ String ],
	     className: String ) =
	       new ReflectionMatcherFactory( name,
					     desc,
					     validParams,
					     paramOrder,
					     className )
  
  /**
   * Creates a new ParseTreeMatcherFactory.
   * @param name The name of the factory
   * @param desc A description of the factory
   * @param validParams Parameters for objects created by the factory
   * @param paramOrder The order of the parameters
   * @param tree The root of the parse tree for the factory
   * @return A new matcher factory that can create these instances
   */
  def apply( name: String,
	     desc: String,
	     validParams: Map[ String, ParamInfo ],
	     paramOrder: Seq[ String ],
	     tree: ParseNode ) =
	       new ParseTreeMatcherFactory( name,
					    desc,
					    validParams,
					    paramOrder,
					    tree )
}

/**
 * Assists in the creation of replacer factories.
 * @author Kyle Dewey
 */
object ReplacerFactory {
  /**
   * Creates a ReflectionReplacerFactory.
   * @param name The name of the factory
   * @param desc A description of the factory
   * @param validParams Parameters for objects created by the factory
   * @param paramOrder The order of the params
   * @param className The JVM class name that backs the instance
   * @return A new replacer factory that can create these instances
   */
  def apply( name: String,
	     desc: String,
	     validParams: Map[ String, ParamInfo ],
	     paramOrder: Seq[ String ],
	     className: String ) =
	       new ReflectionReplacerFactory( name,
					      desc,
					      validParams,
					      paramOrder,
					      className )
  
  /**
   * Creates a new ParseTreeReplacerFactory.
   * @param name The name of the factory
   * @param desc A description of the factory
   * @param validParams Parameters for objects created by the factory
   * @param paramOrder The order of the parameters
   * @param tree The root of the parse tree for the factory
   * @return A new replacer factory that can create these instances
   */
  def apply( name: String,
	     desc: String,
	     validParams: Map[ String, ParamInfo ],
	     paramOrder: Seq[ String ],
	     tree: ParseNode ) =
	       new ParseTreeReplacerFactory( name,
					     desc,
					     validParams,
					     paramOrder,
					     tree )
}

  
