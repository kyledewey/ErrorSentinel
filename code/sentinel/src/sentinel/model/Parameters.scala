/*
 * Parameters.scala
 *
 * Version:
 *     $Id: Parameters.scala,v 1.32 2011/06/21 17:01:38 kyledewey Exp kyledewey $
 *
 * Revisions:
 *      $Log: Parameters.scala,v $
 *      Revision 1.32  2011/06/21 17:01:38  kyledewey
 *      Added the ToConstant trait.
 *      Made Constant and SimpleValue inherit from ToConstant.
 *
 *      Revision 1.31  2011/06/20 22:40:51  kyledewey
 *      Added an equals() and hashCode() method to
 *      NamedParm, based only on the parameter name.
 *
 *      Revision 1.30  2011/06/18 03:28:47  kyledewey
 *      Moved True and False to here.
 *
 *      Revision 1.29  2011/06/17 20:48:16  kyledewey
 *      Added more description for the new parameters setup.
 *
 *      Revision 1.28  2011/06/08 04:23:43  kyledewey
 *      Massive refactor so that all parameter types are uniform
 *      from the perspective of matchers and replacers.
 *
 *      Revision 1.27  2011/06/07 08:15:44  kyledewey
 *      Moved Data into this file.
 *      Added typed replacers.
 *      Added conversion routines for connverting to typed replacers.
 *
 *      Revision 1.26  2011/06/02 00:21:37  kyledewey
 *      Added an equals() and hashCode() method to
 *      the Value trait.
 *
 *      Revision 1.25  2011/05/31 17:17:48  kyledewey
 *      Added code to the CellRange object to allow for
 *      the parsing in of cell ranges from strings.
 *
 *      Revision 1.24  2011/05/31 00:02:59  kyledewey
 *      Added a toString() method to CellRange.
 *
 *      Revision 1.23  2011/05/29 15:21:51  kyledewey
 *      Added the various inRange() methods to CellRange.
 *
 *      Revision 1.22  2011/05/27 18:48:02  kyledewey
 *      Added the CellPointer class.
 *
 *      Revision 1.21  2011/05/27 01:32:13  kyledewey
 *      Added an equals() and hashCode() method to CellRange.
 *
 *      Revision 1.20  2011/05/25 21:52:58  kyledewey
 *      Constant now extends Param.
 *      Added an apply method to Constant that takes the
 *      type of constant to make.
 *
 *      Revision 1.19  2011/05/25 20:04:57  kyledewey
 *      Added foreach() for cell ranges, and code to simplify
 *      the creation of SpreadsheetVariables.
 *
 *      Revision 1.18  2011/04/19 12:59:14  kyledewey
 *      Made replacers and matchers traits instead of
 *      abstract classes.
 *
 *      Revision 1.17  2011/03/27 14:04:37  kyledewey
 *      Added methods to ParamInfo to create copies with
 *      specific changes.
 *
 *      Revision 1.16  2011/02/27 07:48:15  kyledewey
 *      Removed non-working code for the special case of
 *      converting SimpleVariables.
 *
 *      Revision 1.15  2011/02/27 06:39:55  kyledewey
 *      Added the printableValue() method to Param.
 *
 *      Revision 1.14  2011/02/12 02:50:14  kyledewey
 *      Refactored so that there are both Spreadsheet and Simple variables.
 *
 *      Revision 1.13  2010/07/11 05:44:00  kyledewey
 *      Minor shifting of file contents to make the order
 *      match more closely with dependencies.
 *
 *      Revision 1.12  2010/06/26 04:02:24  kyledewey
 *      Made Value extend Ordered.
 *
 *      Revision 1.11  2010/06/25 03:43:57  kyledewey
 *      Changed Int to Long in Replacer's implicit conversions.
 *
 *      Revision 1.10  2010/06/25 03:17:51  kyledewey
 *      Refactored so that variables have types.
 *
 *      Revision 1.9  2010/06/23 03:10:20  kyledewey
 *      Added the sortParams() routine to ParamInfo.
 *
 *      Revision 1.8  2010/06/20 23:36:55  kyledewey
 *      Added more implicit definitions to object Replacer.
 *
 *      Revision 1.7  2010/06/20 23:28:52  kyledewey
 *      Made replace() return type Data, and created
 *      string2Data() method,
 *
 *      Revision 1.6  2010/06/20 17:25:38  kyledewey
 *      Major refactor.  Added typed parameters, and code
 *      to allow for implicit conversions where neccessary.
 *      The language is now a hybrid statically/dynamically
 *      typed language, instead of a purely dynamically
 *      typed language.
 *
 *      Revision 1.5  2010/06/18 19:35:31  kyledewey
 *      Added the to*() and convertTo methods to Param.
 *
 *      Revision 1.4  2010/06/18 03:01:07  kyledewey
 *      Added routines to ParamType to better get the name
 *      of a param or type of a param.
 *
 *      Revision 1.3  2010/06/16 00:58:54  kyledewey
 *      Fixed typo in name of ParameterizedInstantiationException;
 *      Changed getMessage and toString in the exception to provide
 *      more information.
 *
 *      Revision 1.2  2010/06/15 23:58:19  kyledewey
 *      Added typeName() method to Param;
 *      Added toString() to ParamType.
 *
 *      Revision 1.1  2010/06/15 17:53:40  kyledewey
 *      Initial revision
 *
 *
 */

package sentinel.model

/**
 * Contains the types of parameters that are possible
 *
 * @author Kyle Dewey
 */
object ParamType extends Enumeration {
  type ParamType = Value
  val StringType, 
      IntType,
      RealType,
      CharType,
      MatcherType, 
      ReplacerType = Value 

  // begin constants
  val STRING_STRING = "String"
  val INTEGER_STRING = "Integer"
  val REAL_STRING = "Real"
  val CHARACTER_STRING = "Character"
  val VARIABLE_STRING = "Variable"
  val MATCHER_STRING = "Matcher"
  val REPLACER_STRING = "Replacer"
  val paramToString = Map( StringType -> STRING_STRING,
			   IntType -> INTEGER_STRING,
			   RealType -> REAL_STRING,
			   CharType -> CHARACTER_STRING,
			   MatcherType -> MATCHER_STRING,
			   ReplacerType -> REPLACER_STRING )
  val stringToParam = Map() ++ paramToString.map( param =>
    param._2 -> param._1 )
  val instanceTypes = Set( MatcherType,
		           ReplacerType )
  // end constants

  /**
   * Determines if the given type is an instance type.
   * @param theType The type to check
   * @return true if it is an instance type, else false
   */
  def isInstanceType( theType: ParamType ) =
    theType == MatcherType || theType == ReplacerType

  /**
   * Determines if the given string is that of an instance type.
   * @param theType The type to check
   * @return false if the string is not recognized, or it is not
   * that of an instance type
   */
  def isInstanceType( theType: String ): Boolean = {
    if ( stringToParam.contains( theType ) ) {
      isInstanceType( stringToParam( theType ) )
    } else false
  }

  /**
   * Given a parameter type, will convert it to a string
   * @param param The parameter type
   * @return A human-readable string representing this type
   */
  def toString( param: ParamType ): String = 
    paramToString( param )

  /**
   * Returns true if the given function didn't throw an exception,
   * else false.
   * @param function The function to call
   * @return true if it didn't throw an exception, else false
   */
  def falseIfException( function: => Unit ) =
    Constant.callOrNone( function ).isDefined

  /**
   * Determines if the given text looks like a double.
   * Merely tries to parse it as a double.
   * @param text The text to check
   * @return true if it could be parsed to a double, else false
   */
  def looksLikeDouble( text: String ) = 
    falseIfException( java.lang.Double.parseDouble( text ) )

  /**
   * Determines if the given text looks like an integer.
   * Merely tries to parse it as an integer
   * @param text The text to check
   * @return true if it could be parsed to an integer, else false
   */
  def looksLikeInteger( text: String ) =
    falseIfException( java.lang.Integer.parseInt( text ) )

  /**
   * Determines if the given text looks like a character.
   * Merely checks that its length is 1.
   * @param text The text to check
   * @return <code>text.length == 1 </code>
   */
  def looksLikeCharacter( text: String ) =
    text.length == 1

  /**
   * Given a string holding the contents of a value, it attempts
   * to determine what parameter type it best looks like.
   * Note that this is a very gross estimate!
   * @param text The text of the parameter
   * @return The param type it most closely matches
   */
  def looksLike( text: String ) =
    if ( looksLikeInteger( text ) ) IntType
    else if ( looksLikeDouble( text ) ) RealType
    else if ( looksLikeCharacter( text ) ) CharType
    else StringType
}

import ParamType._

/**
 * Holds information about a parameter.  This is relevant
 * to factories which wish to create matchers or replacers.
 * Note that these are intended to represent actual parameters,
 * which is why there is no Param within.  Also note that all
 * parameters are named parameters.
 * 
 * @param name The name of the parameter
 * @param desc A description of the parameter
 * @param paramType The type of the parameter
 * @param array Whether or not it is an array
 * @param required Whether or not it is required
 *
 * @author Kyle Dewey
 */
class ParamInfo( val name: String,
		 val desc: String,
		 val paramType: ParamType,
		 val isArray: Boolean,
		 val isRequired: Boolean ) extends Ordered[ ParamInfo ] {
  /**
   * Creates a ParamInfo object from a tuple.
   * This is done for convenience, as the constructor is a bit lengthy.
   * @param tuple tuple of name, description, param type, whether or
   *        not the item is an array, and whether or not the item is
   *        required
   */
  def this( tuple: Tuple5[ String, String, ParamType, Boolean, Boolean ] ) =
    this( tuple._1,
	  tuple._2,
	  tuple._3,
	  tuple._4,
	  tuple._5 )

  /**
   * Changes the name of this param info object
   * @param newName The new name of the object
   * @return A new ParamInfo object, identical to this with the exception
   * of the name
   */
  def newName( newName: String ) =
    new ParamInfo( newName,
		   desc,
		   paramType,
		   isArray,
		   isRequired )

  /**
   * Changes the description of this object.
   * @param newDesc The new description
   * @return A new object with the given description (copy)
   */
  def newDesc( newDesc: String ) =
    new ParamInfo( name, newDesc,
		   paramType, isArray, isRequired )

  /**
   * Changes the type of this object
   * @param newType The new type to use
   * @return A new copy, with the given type
   */
  def newType( newType: ParamType ) =
    new ParamInfo( name, desc,
		   newType, isArray, isRequired )

  /**
   * Changes whether or not this is an array
   * @param newIsArray the new isArray value
   * @return A new copy, with the given isArray value
   */
  def newIsArray( newIsArray: Boolean ) =
    new ParamInfo( name, desc,
		   paramType, newIsArray, isRequired )

  /**
   * Changes whether or not this parameter is required
   * @param newIsRequired The new isRequired value
   * @return A new copy, with the given isRequired value
   */
  def newIsRequired( newIsRequired: Boolean ) =
    new ParamInfo( name, desc,
		   paramType, isArray, newIsRequired )

  /**
   * Compares this ParamInfo object to another.  The comparison
   * is based on the name.
   * @param other The other ParamInfo object to compare to
   * @return <code>this.name.compare( other.name )</code>
   */
  override def compare( other: ParamInfo ) =
    name.compare( other.name )

  /**
   * Converts this ParamInfo object to a string.
   * @return A string representing this ParamInfo object
   */
  override def toString() = {
    "Name: " + name + "\n" +
    "Description: " + desc + "\n" +
    "Type: " + ParamType.toString( paramType  ) + "\n" +
    "Can be array: " + isArray + "\n" +
    "Is required: " + isRequired
  }
}

/**
 * Contains helper methods for manipulating
 * ParamInfo objects.
 *
 * @author Kyle Dewey
 */
object ParamInfo {
  /**
   * Creates a map of ParamInfo objects, given a sequence
   * of tuples of information about them.
   * @param infos A series of tuples describing elements of the resultant
   *        map
   * @return A map, where there is one element for each item in the sequence.
   *         The keys are the name of the param, and the values are ParamInfo
   *         objects
   */
  def apply( infos: Seq[ ( String, 
			   String, 
			   ParamType, 
			   Boolean, 
			   Boolean ) ] ) = {
    Map() ++ infos.map( tuple =>
      tuple._1 -> new ParamInfo( tuple ) )
  }

  /**
   * Given a bunch of params, will return them such that
   * all required params are first, and in abc order, followed
   * by all optional params in abc order.
   * @param params The params to sort
   * @return The same params, in a different order
   */
  def sortParams( params: Seq[ ParamInfo ] ): Seq[ ParamInfo ] = {
    val ( required,
	  optional ) = params.partition( _.isRequired )
    required.toList.sort( _ < _ ) ++ optional.toList.sort( _ < _ )
  }
}

/**
 * <p>Represents a parameter.  Basically, this is anything that in some
 * way, shape, or form returns a value.</p>
 * <p>There are two similarly named types of methods: *Value() and to*().
 * The *Value() methods are intended to be used at runtime to get the
 * value of items.  The to*() methods are intended to be used at compile time
 * (parse tree parsing) in type conversions that can be done statically.</p>
 * <p>Note that the conversion methods with to*() exist for optimization
 * purposes. It will always be correct to return the parameter as-is.  However,
 * returning None indicates that the given conversion is always impossible,
 * which is relevant to static typing.</p>
 * @author Kyle Dewey
 */
trait Param {
  import ParamType._

  /**
   * Gets the underlying data of this parameter as a string
   * @return The underlying data as a string.  Note that the default
   * is to throw an exception.
   */
  def sentStringValue(): String = {
    throw new ValueException( "Could not convert parameter with type " +
			      typeName + " to a string." )
  }

  /**
   * Gets the underlying data of this parameter as an integer. (long)
   * @return The underlying data as a long.  Note that the default is to
   * parse the string value as a long.
   */
  def sentIntValue() = {
    try {
      java.lang.Long.parseLong( sentStringValue )
    } catch {
      case e: VariableDereferenceException =>
	throw new ValueException( e.getMessage )
      case e: NumberFormatException =>
	throw new ValueException( "Could not convert \"" +
				  sentStringValue + "\" to an integer" )
    }
  }

  /**
   * Like <code>sentIntValue</code>, but with reals (double).
   * @return The value as a double.  Note that the default is to parse
   * the string value as a real.
   */
  def sentRealValue() = {
    try {
      java.lang.Double.parseDouble( sentStringValue )
    } catch {
      case e: VariableDereferenceException =>	throw new ValueException( e.getMessage )
      case e: NumberFormatException =>
	throw new ValueException( "Could not convert \"" +
				  sentStringValue + "\" to a real number" )
    }
  }

  /**
   * Like <code>sentIntValue</code>, but it returns the data as a char.
   * @return The data as a char.  Note that this simply returns the first
   * character of the underlying string.  If the string contains more than
   * a single character, this throws an execption.
   */
  def sentCharValue() = {
    try {
      val string = sentStringValue
      if ( string.length == 1 ) string.charAt( 0 )
      else throw new ValueException( "Could not convert \"" +
				     string + "\" to a character" )
    } catch {
      case e: VariableDereferenceException =>
	throw new ValueException( e.getMessage )
    }
  }

  /**
   * Returns a matcher representation of this.
   * @return This as a matcher.  If it's not already a
   * matcher, this throws an exception.
   */
  def matcherValue(): Matcher = {
    val matcher = toMatcher
    if ( matcher.isDefined &&
         matcher.get.isInstanceOf[ Matcher ] ) {
      matcher.get.asInstanceOf[ Matcher ]
    } else {
      throw new ValueException( "Could not convert param to a matcher." )
    }
  }

  /**
   * Returns a replacer representation of this.
   * @return This as a replacer.  If it's not already a replacer,
   * this throws an exception.
   */
  def replacerValue(): Replacer = {
    val replacer = toReplacer
    if ( replacer.isDefined &&
	 replacer.get.isInstanceOf[ Replacer ] ) {
      replacer.get.asInstanceOf[ Replacer ]
    } else {
      throw new ValueException( "Could not convert param to a replacer." )
    }
  }

  /**
   * Converts this to a string representation, in case there is a more efficient
   * representation than the current representation.
   * @return A representation that may more closely resemble that of a string.
   * The default is <code>Some( this )</code>
   */
  def toSentString(): Option[ Param ] =
    Some( this )

  /**
   * Like <code>toSentString</code>, but with integers (long).
   * @return <code>Some( this )</code>
   */
  def toSentInt(): Option[ Param ] =
    Some( this )

  /**
   * Like <code>toSentString</code>, but with reals (double)
   * @return <code>Some( this )</code>
   */
  def toSentReal(): Option[ Param ] =
    Some( this )

  /**
   * Like <code>toSentString</code>,  but with characters.
   * @return <code>Some( this )</code>
   */
  def toSentChar(): Option[ Param ] =
    Some( this )

  /**
   * Like <code>toSentString</code>, but with replacers.
   * Note that the default differs slightly.
   * @return <code>Some( new VerbatimReplacer( this ) )</code>
   */
  def toReplacer(): Option[ Param ] =
    Some( new VerbatimReplacer( this ) )

  /**
   * Like <code>toSentString</code>, but with matchers.
   * Note that the default differs.
   * @return <code>None</code>
   */
  def toMatcher(): Option[ Param ] =
    None

  /**
   * Converts to an arbitrary type.
   * Note that this will simply dispatch to the proper underlying conversion
   * routine.
   * @param toType The type to convert to
   * @return A converted representation
   */
  def convertTo( toType: ParamType ): Option[ Param ] =
    toType match {
      case StringType => toSentString
      case IntType => toSentInt
      case RealType => toSentReal
      case CharType => toSentChar
      case MatcherType => toMatcher
      case ReplacerType => toReplacer
      case _ => None
    }

  /**
   * Gets the preferred type of this param.
   * @return The type of this param
   */
  def getType(): ParamType

  /**
   * Determines if this is a instance type.
   * Instance types can be created by users, and are flexible.
   * @return false by default
   */
  def isInstanceType() = false

  /**
   * Determines if this is a constant type.
   * Constant types are not as flexible, but they contain strong
   * typing information that leads to more reliable code.
   * @return false by default
   */
  def isConstantType() = false

  /**
   * Determines if this is a variable type.
   * The values of variable types can change like instances, but
   * they are not instances.
   * @return false by default
   */
  def isVariableType() = false

  /**
   * Gets the name of the preferred type of this param.
   * @return The name of the type of this param
   */
  def typeName(): String =
    ParamType.toString( getType )

  /**
   * Converts this parameter to a string.
   * Attempts to use the value of the parameter.
   * @return A string representing this parameter.  If it fails to get
   * the value for whatever reason, then it will return None
   */
  def printableValue() = 
    Constant.callOrNone( printableValueUnsafe )

  def printableValueUnsafe() =
    sentStringValue
}

/**
 * A parameter with the underlying type of SentInt
 * @author Kyle Dewey
 */
trait SentInt extends Param with Ordered[ SentInt ] {
  def getType() = IntType
  def compare( other: SentInt ) =
    sentIntValue.compare( other.sentIntValue )
}

/**
 * Like <code>SentInt</code>, but it is statically typed.
 * Related methods are overridden for efficiency.
 * @author Kyle Dewey
 */
trait StaticSentInt extends SentInt {
  def staticIntValue(): Long
  override def sentStringValue() =
    staticIntValue.toString
  override def sentIntValue() = 
    staticIntValue
  override def sentRealValue() = 
    staticIntValue.asInstanceOf[ Double ]
}

/**
 * A parameter with the underlying type of SentString.
 * @author Kyle Dewey
 */
trait SentString extends Param with Ordered[ SentString ] {
  def getType() = StringType 
  def compare( other: SentString ) =
    sentStringValue.compare( other.sentStringValue )
}

/**
 * Like <code>SentString</code>, but it is statically typed.
 * Related methods are overridden for efficiency.
 * @author Kyle Dewey
 */
trait StaticSentString extends SentString {
  def staticStringValue(): String
  override def sentStringValue() =
    staticStringValue
}

/**
 * A parameter with the underlying type of SentChar.
 * @author Kyle Dewey
 */
trait SentChar extends Param with Ordered[ SentChar ] {
  def getType() = CharType
  def compare( other: SentChar ) =
    sentCharValue.compare( other.sentCharValue )
}

/**
 * Like <code>SentChar</code>, but it is statically typed.
 * Related methods are overridden for efficiency.
 * @author Kyle Dewey
 */
trait StaticSentChar extends SentChar {
  def staticCharValue(): Char
  override def sentStringValue() =
    "" + staticCharValue
  override def sentCharValue() =
    staticCharValue
}

/**
 * A parameter with the underlying type of SentReal.
 * @author Kyle Dewey
 */
trait SentReal extends Param with Ordered[ SentReal ] {
  def getType() = RealType
  def compare( other: SentReal ) =
    sentRealValue.compare( other.sentRealValue )
}

/**
 * Like <code>SentReal</code>, but it is statically typed.
 * Related methods are overridden for efficiency.
 * @author Kyle Dewey
 */
trait StaticSentReal extends SentReal {
  def staticRealValue(): Double
  override def sentRealValue() =
    staticRealValue
}

/**
 * Represents a parameter with a name.
 * Note that in most cases, the underlying parameter
 * should not change.  This is neccessary because parameters
 * often need to be renamed, as in parsing a parse tree.
 * Conversely, not all parameters need names, so it doesn't
 * make sense to put that information inside of Param.
 * @param name The name of the parameter
 * @param param The parameter
 * @author Kyle Dewey
 */
class NamedParam( val name: String, val param: Param ) {
  /**
   * Simulates renaming this parameter.
   * A new NamedParam with the new name and the same
   * underlying param is returned instead.
   * @param newName The new name
   * @return a new NamedParam, with the new name but the same
   *         underlying param
   */
  def rename( newName: String ): NamedParam =
    new NamedParam( newName, param )

  /**
   * Determines if one named param equal another.
   * The determination is based soley on the name.
   * @param other The other object to compare to
   * @return true if the names equal, else false
   */
  override def equals( other: Any ) =
    other != null &&
    other.isInstanceOf[ NamedParam ] &&
    other.asInstanceOf[ NamedParam ].name == name
  
  /**
   * Gets the hash code of this named parameter.
   * Based solely on the name.
   * @return <code>name.hashCode</code>
   */
  override def hashCode() =
    name.hashCode
}

/**
 * Exception thrown when a value could not be returned for
 * whatever reason.
 * @param message A message as to why the value could not be returned
 * @author Kyle Dewey
 */
case class ValueException( message: String ) extends Exception( message ) {}

/**
 * Exception thrown when a parameter with a given name
 * doesn't exist.
 *
 * @param message A message describing the error
 * 
 * @author Kyle Dewey
 */
case class ParameterNameException( message: String ) 
     extends Exception( message ) {}

/**
 * Contains helper routines for ParameterTypeException.
 * @author Kyle Dewey
 */
object ParameterTypeExceptionHelpers {
  import ParamType._

  /**
   * Given a value and it's type, it will return the inner portion
   * of the message for makeMessage.
   * @param found The type that was found
   * @param value The value that was found
   * @return A string of the form " with value <value>", or a null string
   * if the value is empty.  Encloses strings in quotes.
   */
  def makeMessage( found: ParamType, value: Option[ String ] ): String = {
    if ( value.isDefined ) {
      val inner =
	if ( found == StringType ) {
	  "\"" + value.get + "\"" 
	} else {
	  value.get
	}
      " with value " + inner
    } else {
      ""
    }
  }

  /**
   * Given the type found, the value found, and the expected type,
   * makes an error message that can be used for a ParameterTypeException.
   * @param found The type that was found
   * @param value The value here
   * @param expected The expected type
   * @return An error message for a ParameterTypeException
   */
  def makeMessage( found: ParamType,
		   value: Option[ String ],
		   expected: ParamType ): String = {
    "Found type " + ParamType.toString( found ) +
    makeMessage( found, value ) + ". Expected type " +
    ParamType.toString( expected )
  }

  /**
   * Given the named param that triggered the exception and the expected
   * type, creates a message
   * @param param The parameter that triggered the exception
   * @param expected The expected type
   * @return An error message for ParameterTypeException
   */
  def makeMessage( param: NamedParam, expected: ParamType ): String =
    makeMessage( param.param.getType,
		 param.param.printableValue,
		 expected )
}

/**
 * Exception thrown when a parameter's expected type
 * doesn't match the actual type
 * @param message A message describing the error
 * @param param Named parameter that triggered the exception
 * @author Kyle Dewey
 */
case class ParameterTypeException( message: String, val param: NamedParam )extends Exception( message ) {
  import ParamType._

  /**
   * Creates a new exception given the parameter that triggered the exception
   * and the expected type
   * @param param The parameter that triggered the exception
   * @param expected The expected type
   */
  def this( param: NamedParam, expected: ParamType ) =
    this( ParameterTypeExceptionHelpers.makeMessage( param, expected ),
	  param )
}

/**
 * Exception thrown when a required parameter was not passed.
 * @param message A message describing the error
 * @author Kyle Dewey
 */
case class ParameterRequirementException( message: String )
     extends Exception( message ) {}

/**
 * Exception thrown when the type of a parameter was correct,
 * but it was (or was not) as an array when it was supposed
 * to be
 * @param message A message describing the error
 * @author Kyle Dewey
 */
case class ParameterArrayException( message: String ) 
     extends Exception( message ) {}

/**
 * Exception thrown when parameters were correct, but for
 * some reason an object could not be instantiated.
 * @param exception The exception that triggered this to be thrown
 * @param message The message that triggered this exception
 * @author Kyle Dewey
 */
case class ParameterizedInstantiationException( val exception: Exception,
					        message: String ) 
     extends Exception( message ) {
  /**
   * Synonym with the constructor, in case the order is messed up.
   * @param message The message
   * @param exception The underlying exception
   */
  def this( message: String, exception: Exception ) =
    this( exception, message )
       
  /**
   * Creates a new ParameterizedInstantiationException with the given
   * exception at the base, and no message
   * 
   * @param exception The underlying exception
   */
  def this( exception: Exception ) =
    this( exception, null )

  /**
   * Creates a new exception with the given message, and no
   * exception.
   * @param message The message
   */
   def this( message: String ) =
     this( message, null )
       
  /**
   * Gets the message of the underlying exception,
   * and/or the message given to this
   * @return the underlying exception's message
   */
  override def getMessage() = {
    if ( message != null &&
	 exception.getMessage != null ) {
      message + "; " + exception.getMessage
    } else if ( message != null ) {
      message
    } else {
      exception.getMessage
    }
  }

  /**
   * Gets this as a string plus the underlying message
   * as a string.
   * @return the underlying exception as a string
   */
  override def toString() = 
    super.toString + "; " + exception.toString
}

/**
 * All the to*() methods return constant representation of the
 * current *Value() methods.
 * @author Kyle Dewey
 */
trait ToConstant extends Param {
  import Constant._

  override def toSentInt() = 
    callOrNone( Constant( sentIntValue ) )

  override def toSentReal() = 
    callOrNone( Constant( sentRealValue ) )

  override def toSentChar() = 
    callOrNone( Constant( sentCharValue ) )

  override def toSentString() = 
    callOrNone( Constant( sentStringValue ) )
}

/**
 * Represents a constant.
 * Constants have fixed values, which cannot be changed after being set
 * @author Kyle Dewey
 */
trait Constant[ T ] extends ToConstant {
  def value(): T
  override def isConstantType() = true
}

class StringConstant( val value: String ) 
extends Constant[ String ] with StaticSentString {
  def staticStringValue() = value
}

class IntConstant( val value: Long )
extends Constant[ Long ] with StaticSentInt {
  def staticIntValue() = value
}

class RealConstant( val value: Double )
extends Constant[ Double ] with StaticSentReal {
  def staticRealValue() = value
}

class CharConstant( val value: Char )
extends Constant[ Char ] with StaticSentChar {
  def staticCharValue() = value
}

/**
 * Creates constants of the appropriate type, depending on
 * what parameter was passed.
 * @author Kyle Dewey
 */
object Constant {
  /**
   * Wraps the given function call in a try block.
   * If the call threw an exception, this returns None.
   * @param function The function to wrap
   * @return The result of the function, or None if it threw an exception
   */
  def callOrNone[ T ]( function: => T ) = {
    try {
      Some( function )
    } catch {
      case e: Exception => None
    }
  }

  /**
   * Creates a new constant of the given type with the given value.
   * If the type is invalid, None is returned
   * @param theType The type of the constant to make
   * @param value The value for the constant
   * @return A constant of the given type holding the given value.  Returns
   * None if the format conversion was impossible
   */
  def apply( theType: ParamType, value: String ): Option[ Param ] = 
    apply( value ).convertTo( theType )


  /**
   * Creates a new string constant
   * @param value The string to put into the constant
   * @return A new string constant
   */
  def apply( value: String ) =
    new StringConstant( value )

  /**
   * Creates a new real constant.
   * @param value The double to put into the constant
   * @return A new real constant
   */
  def apply( value: Double ) = 
    new RealConstant( value )

  /**
   * Creates a new character constant
   * @param value The character to put into the constant
   * @return A new character constant
   */
  def apply( value: Char ) =
    new CharConstant( value )

  /**
   * Creates a new integer constant
   * @param value The integer to put into the constant
   * @return A new integer constant
   */
  def apply( value: Long ) =
    new IntConstant( value )
}

/**
 * Exception thrown when an attempt to access a nonexistent
 * spreadsheet/row/column is attempted.
 * @param message A useful message to show the user
 * @author Kyle Dewey
 */
case class VariableDereferenceException( message: String ) extends Exception {}

/**
 * Exception thrown when a parameter passed to a variable is invalid.
 * @param message A useful message to show the user
 * @author Kyle Dewey
 */
case class VariableArgumentException( message: String )
     extends IllegalArgumentException {}

/**
 * Represents a variable.  Variables have values which can change over time.
 * @author Kyle Dewey
 */
trait Variable extends Param {
  override def isVariableType() = true
}

/**
 * A simple variable that contains the very data it's going to
 * give
 * @param variable The data to return
 * @author Kyle Dewey
 */
abstract class SimpleVariable[ T ]( var variable: T ) 
extends Variable with ToConstant {}

/**
 * A simple variable with the underlying type of SentString.
 * @param _variable The initial value
 * @author Kyle Dewey
 */
class SimpleStringVariable( _variable: String ) 
extends SimpleVariable[ String ]( _variable ) with StaticSentString {
  def staticStringValue() = variable
}

/**
 * A simple variable with the underlying type of SentChar.
 * @param _variable The initial value
 * @author Kyle Dewey
 */
class SimpleCharVariable( _variable: Char ) 
extends SimpleVariable[ Char ]( _variable ) with StaticSentChar {
  def staticCharValue() = variable
}

/**
 * A simple variable with the underlying type of SentInt.
 * @param _variable The initial value
 * @author Kyle Dewey
 */
class SimpleIntVariable( _variable: Long ) 
extends SimpleVariable[ Long ]( _variable ) with StaticSentInt {
  def staticIntValue() = variable
}

/**
 * A simple variable with the underlying type of SentReal.
 * @param _variable The initial value
 * @author Kyle Dewey
 */
class SimpleRealVariable( _variable: Double )
extends SimpleVariable[ Double ]( _variable ) with StaticSentReal {
  def staticRealValue() = variable
}

/**
 * Refers to a single cell in a spreadsheet.
 * Unlike CellRange, this is guarenteed to be instantiated.
 * @param sheet The sheet this applies to
 * @param row The row this applies to
 * @param column The column this applies to
 * @author Kyle Dewey
 */
case class CellPointer( val sheet: String,
		        val row: Int,
		        val column: Int ) {}

/**
 * Holds constants applicable to cell ranges.
 * @author Kyle Dewey
 */
object CellRange {
  import Spreadsheet._
  /**
   * if the given string equals the given value, this returns
   * None.  Otherwise it returns Some( String )
   * @param value The string value
   * @param compare The value to compare against
   * @return Some( value ) if value != compare, else None
   */
  def valueOrNone( value: String, compare: String ) =
    if ( value == compare ) {
      None
    } else {
      Some( value )
    }

  /**
   * Parses a sheet as a string.
   * If the sheet is ANY_SHEET, then it returns none
   * @param sheet The sheet as a string
   * @return Either Some( sheet ) or None.  The value is valid for CellRange
   */
  def parseSheet( sheet: String ) =
    valueOrNone( sheet, ANY_SHEET )

  /**
   * Parses in the given value as an integer.
   * It works like valueOrNone as well.
   * @param string The string
   * @param compare The string to compare to
   * @return A parameter for CellRange
   * @throws NumberFormatException If the given row isn't an integer
   */
  def parseInt( string: String, compare: String ) = {
    val first = valueOrNone( string, compare )
    if ( first.isDefined ) {
      Some( Integer.parseInt( first.get ) ) 
    } else {
      None
    }
  }

  /**
   * Parses in a row as a string.
   * If the row is ANY_ROW, this returns None.
   * @param row The row as a string
   * @return A parameter for CellRange
   * @throws NumberFormatException If the given row isn't an integer
   */
  def parseRow( row: String ) = 
    parseInt( row, ANY_ROW )

  /**
   * Parses in a column as a string.
   * If the column is ANY_COLUMN, this returns None
   * @param column The column as a string
   * @return A parameter for CellRange
   * @throws NumberFormatException If the given column isn't an integer
   */
  def parseColumn( column: String ) =
    parseInt( column, ANY_COLUMN )

  /**
   * Parses in a cell range from a string.
   * @param string The string holding a cell range
   * @return A cell range representation of the string, or None if
   * such a range could not be made.
   */
  def parseCellRange( string: String ) = {
    val split = string.split( DELIMETER )
    if ( split.length == 3 ) {
      try {
	Some( new CellRange( parseSheet( split( 0 ) ),
			     parseRow( split( 1 ) ),
			     parseColumn( split( 2 ) ) ) )
      } catch {
	case e: NumberFormatException => None
      }
    } else {
      None
    }
  }
}

/**
 * Holds a range of cells across a range of sheets.
 * @param sheet The sheet the range applies to.  Specify None for all sheets.
 * @param row The row the range applies to.  Specify None for all rows.
 * @param column The column the range applies to.  Sprecify None for all
 * columns
 * @author Kyle Dewey
 */
class CellRange( val sheet: Option[ String ],
		 val row: Option[ Int ],
		 val column: Option[ Int ] ) {
  /**
   * Determines if the given sheet is in the range of this cell range.
   * @param theSheet The sheet name to check
   * @return true if it's in range, else false
   */
  def sheetInRange( theSheet: String ) =
    if ( sheet.isDefined ) {
      sheet.get == theSheet
    } else {
      true 
    }

  /**
   * Determines if the given row is in the range of this cell range.
   * @param theRow The row
   * @return true if it's in range, else false.
   */
  def rowInRange( theRow: Int ) = 
    if ( row.isDefined ) {
      row.get == theRow
    } else {
      true
    }
  
  /**
   * Determines if the given column is in this cell range.
   * @param theColumn The Column
   * @return true if it's in range, else false
   */
  def columnInRange( theColumn: Int ) =
    if ( column.isDefined ) {
      column.get == theColumn
    } else {
      true
    }

  /**
   * Determines if the given sheet, row, and column is
   * within this cell range.  This is O(1).
   * @param sheet The the sheet.
   * @param row The row
   * @param column The column
   * @return true if the given info is in the range, else false
   */
  def inRange( sheet: Spreadsheet, row: Int, column: Int ): Boolean = 
    ( rowInRange( row ) &&
      columnInRange( column ) &&
      sheetInRange( sheet.name ) &&
      sheet.inRange( row, column ) )
    
  /**
   * Like <code>inRange</code>, but the sheet is specified as
   * a sheet name.  Note that if the given sheet doesn't
   * exist, this returns false.
   * @param sheet The name of the sheet
   * @param row The row
   * @param column The column
   * @return true if the given info is in range, else false
   */
  def inRange( sheet: String, row: Int, column: Int ): Boolean = {
    val asSpread = Spreadsheet.getSpreadsheet( sheet )
    if ( asSpread.isDefined ) {
      inRange( asSpread.get, row, column )
    } else {
      false 
    }
  }
  
  /**
   * Executes the given function for each cell defined by this cell
   * range.  Notes that it will use Spreadsheet's getSpreadsheets
   * if the sheet isn't defined.
   * @param function The function to execute.  Takes in the current sheet,
   * row, and column
   */
  def foreach( function: ( String, Int, Int ) => Unit ) {
    import Spreadsheet._
    sheets.foreach( sheet => {
      val spreadsheet = getSpreadsheet( sheet )
      if ( spreadsheet.isDefined ) {
	val rows =
	  if ( row.isDefined ) Seq( row.get )
	  else 0.until( spreadsheet.get.getRowCount ).toSeq
	val columns =
	  if ( column.isDefined ) Seq( column.get )
	  else 0.until( spreadsheet.get.getColumnCount ).toSeq
	rows.foreach( row =>
	  columns.foreach( column => 
	    function( sheet, row, column ) ) )
      }
    } )
  }
  
  /**
   * Gets the sheets that this cell range applies to.
   * Note that this cannot be done for rows and columns, as that depends
   * on the individual sheet
   * @return The sheets that this cell range applies to
   */
  def sheets() =
    if ( sheet.isDefined ) Seq( sheet.get )
    else Spreadsheet.getSpreadsheets.map( _.self )

  /**
   * Makes sure that the sheet, row, and column all have values.
   * If they don't currently have values, then the current value of
   * each will be used.
   * @return A new cell pointer that holds either these values
   * or the values of the current items in Spreadsheet
   */
  def cellPointer() =
    new CellPointer( sheet.getOrElse( Spreadsheet.currentSpreadsheet ),
		     row.getOrElse( Spreadsheet.currentRow ),
		     column.getOrElse( Spreadsheet.currentColumn ) )
  
  /**
   * Determines if this cell range equals another.
   * This means that the sheet, row, and column are the same.
   * @param other The other object to compare to
   */
  override def equals( other: Any ) = 
    if ( other != null &&
	 other.isInstanceOf[ CellRange ] ) {
      val asCell = other.asInstanceOf[ CellRange ]
      ( sheet == asCell.sheet &&
        row == asCell.row &&
        column == asCell.column )
    } else {
      false 
    }

  /**
   * Gets the hash code of this cell range.
   * Simply the addition of the items within
   * @return The additive hashcode of the contents of this cell range
   */
  override def hashCode() =
    sheet.hashCode + row.hashCode + column.hashCode 

  /**
   * Gets a human-readable description of this cell range.
   * @return A human-readable description of the contents
   */
  override def toString() = {
    import Spreadsheet._
    import CellRange._
    sheet.getOrElse( ANY_SHEET ) + DELIMETER +
      row.getOrElse( ANY_ROW ) + DELIMETER +
      column.getOrElse( ANY_COLUMN )
  }
}

/**
 * Exception thrown when we don't recognize the type of a spreadsheet variable.
 * @param message A message to show the user
 * @author Kyle Dewey
 */
case class UnknownSpreadsheetVariableType( message: String )
     extends Exception( message ) {}

/**
 * <p>Represets a variable. Variables are essentially pointers to cells
 * in the spreadheet.  They allow for the rest of the error correction
 * language to be completely independent of spreadsheets.</p>
 * <p>Note that variables can be set to a fixed place in a spreadsheet,
 * as in mySheet:3:2.  In this example, the spreadsheet is mySheet, the
 * row is #3, and the column is #2.  Indexes start at 0.</p>
 * <p>Of course, the power of a variable is in it's ability to change.  This
 * is permitted by having a concept of a current sheet, row, and column.  To
 * specify the current, merely pass None in the appropriate argument.  For
 * instance, *:*:* refers to the current spreasheet, the current row, and
 * the current column.</p>
 * <p>Note that all uses of current are not valid.  Although programmatically
 * there is always a current sheet, row, and column, certain combinations
 * which are error-prone are disallowed.  For instance, if given something
 * like: *:4:3, this will generate a VariableArgumentException.  This variable
 * points to row 4 and column 3 of the current spreadsheet, which is
 * extremely dangerous.  (Proper usage would mean that the value at row 4
 * and column 3 is fixed for all spreadsheets, which, even if it were true,
 * would be indicative of very bad practice which should in no way be
 * encouraged.)  It is always acceptable to point to the current row and
 * column.  To get a better idea of what is valid and where, refer to the
 * following list of examples:</p>
 * <table border="1">
 * <tr><td>*:*:*</td><td>Valid</td></tr>
 * <tr><td>MySheet:*:*</td><td>Valid</td></tr>
 * <tr><td>MySheet:*:3</td><td>Valid</td></tr>
 * <tr><td>MySheet:4:*</td><td>Valid</td></tr>
 * <tr><td>MySheet:4:3</td><td>Valid</td></tr>
 * <tr><td>*:4:3</td><td>Invalid</td></tr>
 * </table>
 * <p>More or less, if one wants to reference a specific row and/or
 * column, then the sheet must be defined (not *).</p>  
 * @param sheet The name of the spreasheet we are associated with.
 * Specify None for *.
 * @param row The row of the spreadsheet we are associated with.
 * Specify None for *.
 * @param column The column of the spreadhseet we are associated with.
 * Specify None for *.
 * @throws VariableArgumentException If an attempt to reference a specific
 * row and column was made but a specific spreadsheet wasn't declared.  Also
 * if a negative row or column was given.  Note that spreadsheet name checking
 * isn't done until an attempt to get a spreadsheet value is made, so that
 * spreadsheets and variables can be made in any order (otherwise, spreadsheets
 * would have to be made before variables).
 * @author Kyle Dewey
 */
class SpreadsheetVariable( sheet: Option[ String ], 
			   row: Option[ Int ],
			   column: Option[ Int ] ) 
extends CellRange( sheet, row, column ) with Variable with SentString {
  Variable.verifyArgs( sheet,
		       row,
		       column )

  /**
   * Creates a new spreadsheet variable based off of an existing cell range.
   * @param range The existing cell range
   */
  def this( range: CellRange ) =
    this( range.sheet,
	  range.row,
	  range.column )

  /**
   * Gets the value of the cell in the spreadsheet
   * @return The value of the cell in the spreadsheet specified by where
   * @throws VariableDereferenceException If the given spreadsheet is
   * invalid, or if the row and/or column is invalid
   */
  override def sentStringValue() = {
    import Spreadsheet._
    try {
      val theSheet = getSpreadsheet( sheet )
      if ( theSheet.isDefined ) {
	theSheet.get.getValueAt( row.getOrElse( currentRow ),
				 column.getOrElse( currentColumn ) ).toString
      } else {
	throw new VariableDereferenceException( "Unknown spreadsheet with name: " +
					        sheet.getOrElse( "CURRENT SHEET" ) )
      }
    } catch {
      case e: ArrayIndexOutOfBoundsException =>
	throw new VariableDereferenceException( "Invalid row or column: " +
					        e.getMessage )
    }
  }
}

/**
 * Can create variables of different types.
 * @author Kyle Dewey
 */
object Variable {
  /**
   * Verifies that the arguments passed to the variable are valid
   * @param sheet The sheet that we are associated with
   * @param row The row we are associated with
   * @param column The column we are associated with
   * @throws VariableArgumentException If the row and column is negative,
   * or if the row and/or column is specifically defined but the spreadsheet
   * is not
   */
  def verifyArgs( sheet: Option[ String ],
		  row: Option[ Int ],
		  column: Option[ Int ] ) {
    if ( row.isDefined &&
	 row.get < 0 ) {
      throw new VariableArgumentException( "Variable row cannot be negative" )
    } else if ( column.isDefined &&
	        column.get < 0 ) {
      throw new VariableArgumentException( "Variable column cannot be negative" )
    } else if ( ( row.isDefined || column.isDefined ) &&
	        sheet.isEmpty ) {
      throw new VariableArgumentException( "Specific row and/or column, but no " +
					   "specific spreadsheet" )
    }
  }
}

/**
 * Represents an instance of a class.
 * By far, instances are the most flexible kind of params.
 * Conversely, they are also the most "magical", and misuse
 * can lead to non-robust code.
 * @author Kyle Dewey
 */
trait Instance extends Param {
  /**
   * Gets all parameters that have been passed to this instance.
   * @return All the parameters that were passed to this instance
   */
  def params(): Seq[ NamedParam ]

  /**
   * Gets the class name of this instance.
   * @return The class name of this instance
   */
  def className(): String

  /**
   * Gets whether or not the instance is functionally pure.
   * For pure functions, the same parameters will always yield the same
   * outputs.  There are a slew of optimizations that can be performed
   * on pure functions.  By default, instances in Error Sentinel are pure.
   * @return true
   */
  def isPure() = true
}

/**
 * Exception thrown when a problem occurs in a matcher.
 * @param message A message to show the user
 * @author Kyle Dewey
 */
case class MatchException( message: String ) extends Exception( message ) {}

/**
 * Exception thrown when a problem occurs in a replacer.
 * @param message A message to show the user
 * @author Kyle Dewey
 */
case class ReplaceException( message: String ) extends Exception( message ) {}

/**
 * <p>Base class for anything that wants to act as a matcher.
 * Note that matchers are intended to be created via 
 * factory objects.</p>
 * <p>Note that matchers should have a constructor
 * that takes a String, Seq[ Param ].  The string
 * is the name of the resulting matcher/replacer.  The
 * Seq[ Param ] is a listing of params, which are guarenteed
 * to be valid as long as the following is true:
 * -The matcher/replacer was created through its correpsonding factory
 * -The neccessary params specified in the factory are correct</p>
 *
 * @author Kyle Dewey
 */
trait Matcher extends Instance {
  /**
   * Determines if we have a match, as determined by parameters.
   * @return true if this matcher matches, else false
   * @throws MatchException If no match could be determined
   */
  def matches(): Boolean

  /**
   * Overridden for efficiency.
   * @return <code>Some( this )</code>
   */
  override def toMatcher() =
    Some( this )

  /**
   * Returns <code>matches</code> as a string.
   * @return <code>Some( matches.toString )</code>
   */
  override def printableValueUnsafe() = 
    matches.toString

  /**
   * Gets that this is a matcher.
   * @return That this is a matcher
   */
  override def getType() =
    ParamType.MatcherType
}

/**
 * Matcher that always returns true
 * @param className The name of the class
 * @param params Params to the matcher (takes none)
 * @author Kyle Dewey
 */
class True( val className: String, 
	    val params: Seq[ NamedParam ] ) extends Matcher {
  /**
   * Convenience constructor for use with built-ins.
   * Note that using this means that the class name CANNOT be changed!
   */
  def this() = 
    this( "True", Seq() )

  /**
   * Always returns true
   * @return true
   */
  override def matches() = 
    true
}

/**
 * Matcher that always returns false
 * @param className The name of the class
 * @param params Params to the matcher (takes none)
 * @author Kyle Dewey
 */
class False( val className: String, 
	     val params: Seq[ NamedParam ] ) extends Matcher {
  /**
   * Convenience constructor for use with built-ins.
   * Note that using this means that the class name CANNOT be changed!
   */
  def this() = 
    this( "False", Seq() )

  /**
   * Always returns false
   * @return false
   */
  override def matches() = 
    false
}

/**
 * Object that assists with operations dealing with replacers.
 * @author Kyle Dewey
 */
object Replacer {
  /**
   * Converts a string to data.
   * This is intended for the return value of replace(),
   * where it is very common to return a string.
   * @param string The string to convert
   * @return The string as Data, specifically a StringConstant
   */
  implicit def string2Param( string: String ) =
    Constant( string )

  /**
   * Converts an integer to data.
   * Saves some typing for mathematical operations.
   * @param integer The integer to convert
   * @return The integer as Data, specifically an IntConstant
   */
  implicit def integer2Param( integer: Int ) =
    long2Param( integer.asInstanceOf[ Long ] )

  /**
   * Converts a long to data
   * Saves some typing for mathematical operations
   * @param long The long to convert
   * @return The long as Data, specifically an IntConstant
   */
  implicit def long2Param( long: Long ) =
    Constant( long )

  /**
   * Converts a double to data.
   * Saves some typing for mathematical operations.
   * @param theDouble The double to convert
   * @return The double as data, specifically a RealConstant
   */
  implicit def double2Param( theDouble: Double ) =
    Constant( theDouble )

  /**
   * Converts a character to data.
   * Saves some typing in replacer.
   * @param theChar The character to convert
   * @return The character as data, specifically a CharConstant
   */
  implicit def char2Param( theChar: Char ) =
    Constant( theChar )
}

/**
 * <p>Base class for anything that wants to act as a replacer.
 * Note that replacers are intended to be created via factory
 * objects.</p>
 *
 * <p>Note that replacers only replace; as to why they perform
 * a replacement is irrelevant</p>
 *
 * @author Kyle Dewey
 */
trait Replacer extends Instance {
  /**
   * Gets a replacement.
   * Note that what the replacement is is defined by parameters
   * specific to an individual replacer.  Also note that what
   * a replacer replaces is determined by a matcher.
   * @return The replacement
   * @throws ReplaceException If no replacement could be determined
   */
  def replace(): Param

  /**
   * Overridden to return <code>this</code>
   * @return <code>Some( this )</code>
   */
  override def toReplacer() =
    Some( this )

  /**
   * Delegates to replace.
   * @return <code>replace.sentStringValue</code>
   */
  override def sentStringValue() =
    replace.sentStringValue

  /**
   * Delegates to replace.
   * @return <code>replace.sentIntValue<code>
   */
  override def sentIntValue() =
    replace.sentIntValue

  /**
   * Delegates to replace.
   * @return <code>replace.sentCharValue</code>
   */
  override def sentCharValue() =
    replace.sentCharValue

  /**
   * Delegates to replace.
   * @return <code>replace.sentRealValue<code>
   */
  override def sentRealValue() =
    replace.sentRealValue

  /**
   * Gets that this is a replacer.
   * @return That this is a replacer
   */
  override def getType() =
    ParamType.ReplacerType
}

/**
 * A basic replacer that will replace with whatever it was
 * given as a parameter.  This is intended for conversions to
 * replacers.
 * @param toReturn What to return on calling replace()
 * @author Kyle Dewey
 */
class VerbatimReplacer( val replace: Param ) extends Replacer {
  /**
   * The parameters passed are not normal, so this merely returns
   * an empty Seq.
   * @return an empty seq
   */
  def params() = Seq()

  /**
   * Gets the name of the class.
   * @return The name of the class
   */
  def className() = "VerbatimReplacer"
}
