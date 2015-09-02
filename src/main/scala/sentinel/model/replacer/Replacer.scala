/*
 * Replacer.scala
 */

package sentinel.model.replacer

import sentinel.model._
import sentinel.model.matcher.StringHelpers._
import sentinel.model.Replacer._
import sentinel.model.InstanceFactory._

/**
 * Contains helper methods common to different replacers.
 * @author Kyle Dewey
 */
object Replacer {
  def max[ T <: Comparable[ T ] ]( first: T, second: T ): T =
    if ( first.compareTo( second ) > 0 ) first else second

  def min[ T <: Comparable[ T ] ]( first: T, second: T ): T =
    if ( first.compareTo( second ) < 0 ) first else second
	 
  /**
   * Gets the maximum value in the given items.
   * @param items The items to get the max of
   * @return The maximum value in the items
   * @throws ValueException If we could not get the value of an element
   * @pre items contains at least one element
   */
  def max[ T <: Comparable[ T ] ]( items: Seq[ T ] ): T = 
    items.reduceLeft( max( _, _ ) )

  /**
   * Gets the miniumum value in the given items.
   * @param items The items to get the minimum of
   * @return The minimum value in the items
   * @throws ValueException If we could not get the value of an element
   * @pre items contains at least one element
   */
  def min[ T <: Comparable[ T ] ]( items: Seq[ T ] ): T =
    items.reduceLeft( min( _, _ ) )
}

/**
 * Replacer that will concatenate its arguments together,
 * and return that as the replacement.
 * If no arguments are specified, then it will return null string
 * @param className The name of the class
 * @param params Params to the replacer
 * <ul><li>"data": one or more of any data</li>
 * <li>"start": Any data to put at the start before concatenation.  Defaults
 * to a null string.</li>
 * <li>"delimiter": Any data to put between each item.  Defaults to a null
 * string.</li>
 * <li>"end": Any data to put at the end of concatentation.  Defaults to
 * a null string.</li></ul>
 * @author Kyle Dewey
 */
class Concat( val className: String,
	      val params: Seq[ NamedParam ] ) extends Replacer {
  private val input = asArray( "data", params )
  private val start = 
    opParam( "start", params ).getOrElse( Constant( "" ) )
  private val delim = 
    opParam( "delimiter", params ).getOrElse( Constant( "" ) )
  private val end = 
    opParam( "end", params ).getOrElse( Constant( "" ) )

  /**
   * Concatenates all given strings
   * @return The concatenation, or null string if there were none
   */
  override def replace() =
    toStrings( input ).mkString( start.sentStringValue,
				 delim.sentStringValue,
				 end.sentStringValue )
}

/**
 * <p>Replacer that acts as a simple conditional <b>expression</b>.
 * Not to be confused with an if statement.<p>
 * 
 * <p>In more detail, this acts as a functional if expression, not an
 * imperative if statement.  The imperative if statement, such as:
 * <code>int x = 0; if( true ) { x++; }</code> does not return anything,
 * and its function is based purely on side effects.  On the other hand,
 * the functional if statement, like: <code>int x = 0; if ( true ) x + 1 else x
 * </code>returns a value under all conditions, and does not rely upon
 * side effects.</p>
 *
 * <p>The difference may seem trivial, but it is a marked difference between
 * functional and imperative languages.  The error correction langauge
 * is object oriented and functional.  Note that the else clause is required,
 * as it is in many ML derived languages.  The reason for this isn't to be
 * "pure", but rather to force the user to understand exactly the consequences
 * of using the power but dangerous if expression.</p>
 *
 * <p>To make things much simpler, this if expression is roughly
 * equivalent to ?: ternary operator found in many imperative, C
 * derived languages.  In a C based languge, this would be like
 * <code>( true ) ? 1 : 0</code>, where in this language it would be
 * like <code>if ( true ) 1 else 0</code></p>
 *
 * @param className The name of the class
 * @param params to the replacer<ul>
 *         <li>"if": A matcher that will determine whether or not the condition
 *         has occurred</li>
 *        <li>"then": Data that will be returned if the condition is true</li>
 *        <li>"else": Data that will be returned if the condition is false</li>
 *        </ul>
 * @author Kyle Dewey
 */
class Conditional( val className: String,
		   val params: Seq[ NamedParam ] ) extends Replacer {
  private val ifMatches = param( "if", params ).matcherValue
  private val thenDo = param( "then", params )
  private val elseDo = param( "else", params )

  /**
   * If the condition is true, then "then" will be executed.  Otherwise,
   * "else" will be executed.
   * @return The replacement, according to the above description
   */
  override def replace() =
    if ( ifMatches.matches ) thenDo else elseDo
}

/**
 * Merely throws a replacer exception.
 * @param className The name of the class
 * @param params Params to the replacer
 * <ul><li>"message": any data to show what happened</li></ul>
 * @author Kyle Dewey
 */
class ThrowReplaceException( val className: String,
			     val params: Seq[ NamedParam ] ) extends Replacer {
  private val message = param( "message", params )

  /**
   * Doesn't actually ever return.
   * Merely throws an exception with whatever message was given.
   * @return nothing
   * @throws ReplaceException Under all cases
   */
  override def replace() =
    throw new ReplaceException( message.sentStringValue )
}
  
/**
 * Simple debugging utility.
 * Merely passes its parameter along, if it got one.  Otherwise it returns
 * a null string.
 * @param className The name of the class
 * @param params Parameters to the replacer<ul>
 * <li>"input": Any data.  Optional.</li></ul>
 * @author Kyle Dewey 
 */
class Debugger( val className: String,
	        val params: Seq[ NamedParam ] ) extends Replacer {
  private val input = opParam( "input", params )

  /**
   * If we have an input, it returns the input as-is.  Otherwise it
   * returns a null string
   * @return The input as-is, or a null string
   */
  override def replace() =
    if ( input.isDefined ) input.get else ""

  /**
   * Uses <code>printableValueUnsafe</code> instead of sentStringValue.
   * This is so input matchers can have their values shown properly.
   * @return A string representation of the given parameter
   */
  override def sentStringValue() =
    replace.printableValueUnsafe
}
