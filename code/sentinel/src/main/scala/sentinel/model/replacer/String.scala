/*
 * String.scala
 * 
 * Version:
 *     $Id: String.scala,v 1.6 2011/06/08 04:26:15 kyledewey Exp $
 *
 * Revisions:
 *      $Log: String.scala,v $
 *      Revision 1.6  2011/06/08 04:26:15  kyledewey
 *      Now conforms to the new Param interface.
 *
 *      Revision 1.5  2011/06/07 08:17:44  kyledewey
 *      Added replacer types to nonpolymorphic routines.
 *
 *      Revision 1.4  2011/05/25 20:14:58  kyledewey
 *      Made it so the class name is also taken as a parameter.
 *
 *      Revision 1.3  2010/06/28 18:43:38  kyledewey
 *      Changed comment for substring that we now end at the length
 *      of the string by default.
 *
 *      Revision 1.2  2010/06/26 16:36:29  kyledewey
 *      Made "first" for Substitution be passed as a matcher
 *      as opposed an integer.
 *
 *      Revision 1.1  2010/06/26 04:05:40  kyledewey
 *      Initial revision
 *
 *
 */

package sentinel.model.replacer

import sentinel.model._
import sentinel.model.matcher.StringHelpers._
import sentinel.model.InstanceFactory._
import sentinel.model.Replacer._

class StringMax( val className: String,
		 val params: Seq[ NamedParam ] ) extends Replacer {
  private val strings = asArray( "strings", params )
  override def replace() =
    Replacer.max( toStrings( strings ) ).toString
}

class StringMin( val className: String,
		 val params: Seq[ NamedParam ] ) extends Replacer {
  private val strings = asArray( "strings", params )
  override def replace() =
    Replacer.min( toStrings( strings ) ).toString
}

/**
 * Gets an element of the given string.
 * @param className The name of the class
 * @param params Params to the replacer
 * <ul><li>"string": The string to get the position of</li>
 * <li>"position": The position of the desired character</li></ul>
 * @author Kyle Dewey
 */
class CharAt( val className: String,
	      val params: Seq[ NamedParam ] ) extends Replacer {
  private val string = param( "string", params )
  private val pos = param( "position", params )

  /**
   * Gets the character at the given position.
   * @return The character at the given position
   * @throws ReplaceException If the index is invalid
   */
  override def replace() = {
    try {
      string.sentStringValue.charAt( pos.sentIntValue.asInstanceOf[ Int ] )
    } catch {
      case e: IndexOutOfBoundsException =>
	throw new ReplaceException( "For string \"" + string.sentStringValue +
				    "\", " + pos.sentIntValue + " is not a " +
				    "valid index" )
    }
  }
}

/**
 * Gets a substring of a string
 * @param className The name of the class
 * @param params Params to the replacer
 * <ul><li>"string": The string to get the substring of</li>
 * <li>"start": optional start position; defaults to 0</li>
 * <li>"end": optional end position; defaults to the length of the string.</li>
 * </ul>
 * @author Kyle Dewey
 */
class SubString( val className: String,
		 val params: Seq[ NamedParam ] ) extends Replacer {
  private val string = param( "string", params )
  private val start = 
    opParam( "start", params ).getOrElse( Constant( 0 ) )
  private val end = opParam( "end", params )

  /**
   * Gets a substring from the string
   * @return The substring
   * @throws ReplaceException If an index is invalid
   */
  override def replace() = {
    lazy val stringValue = string.sentStringValue
    val endPos = end.getOrElse( Constant( stringValue.length ) )
    try {
      stringValue.substring( start.sentIntValue.asInstanceOf[ Int ],
		             endPos.sentIntValue.asInstanceOf[ Int ] )
    } catch {
      case e: IndexOutOfBoundsException =>
	throw new ReplaceException( e.toString )
    }
  }
}

/**
 * Gets the length of a string.
 * @param className The name of the class
 * @param params Params to the replacer
 * <ul><li>"string": The string to get the length of</li></ul>
 * @author Kyle Dewey
 */
class Length( val className: String,
	      val params: Seq[ NamedParam ] ) extends Replacer {
  private val string = param( "string", params )
  
  /**
   * Gets the length of the string.
   * @return The length of the string
   */
  override def replace() =
    string.sentStringValue.length
}

/**
 * Gets the reverse of the given string.
 * @param className The name of the class
 * @param params Params to the replacer
 * <ul><li>"string": The string to get the reverse of</li></ul>
 * @author Kyle Dewey
 */
class Reverse( val className: String,
	       val params: Seq[ NamedParam ] ) extends Replacer {
  private val string = param( "string ", params )

  /**
   * Gets the reverse of the string
   * @return The reverse of the string
   */
  override def replace() =
    string.sentStringValue.reverse.toString
}

/**
 * Trims whitespace from the front and back of a string.
 * @param className The name of the class
 * @param params Params to the replacer
 * <ul><li>"string": The string to drop the whitespace from the ends</li></ul>
 * @author Kyle Dewey
 */
class Trim( val className: String,
	    val params: Seq[ NamedParam ] ) extends Replacer {
  private val string = param( "string", params )

  /**
   * Trims off whitespace from either end of the string
   * @return The string without whitspace on the ends
   */
  override def replace() =
    string.sentStringValue.trim
}

/**
 * Trims whitespace from the lefthanded side of the string
 * @param className The name of the class
 * @param params Params to the replacer
 * <ul><li>"string": The string to drop the whitespace from the left</li></ul>
 * @author Kyle Dewey
 */
class LeftTrim( val className: String,
	        val params: Seq[ NamedParam ] ) extends Replacer {
  private val string = param( "string", params )

  /**
   * Trims whitespace off the left side of the string
   * @return The string without whitespace on the left side
   */
  override def replace() =
    string.sentStringValue.dropWhile( Character.isWhitespace( _ ) ).mkString
}

/**
 * Trims whitespace from the righthand side of the string
 * @param className The name of the class
 * @param params Params to the replacer
 * <ul><li>"string": The string to drop the whitespace from the right</li></ul>
 * @author Kyle Dewey
 */
class RightTrim( val className: String,
		 val params: Seq[ NamedParam ] ) extends Replacer {
  private val string = param( "string", params )

  /**
   * Trims whitespace from the righthand side of the string
   * @return The string without whitespace on the right side
   */
  override def replace() =
    string.sentStringValue
          .reverse
          .dropWhile( Character.isWhitespace( _ ) )
          .reverse
	  .mkString
}

/**
 * Makes the given string uppercase.
 * @param className The name of the class
 * @param params Params to the replacer
 * <ul><li>"string": The string to make uppercase</li></ul>
 * @author Kyle Dewey
 */
class StringMakeUpperCase( val className: String,
			   val params: Seq[ NamedParam ] ) extends Replacer {
  private val string = param( "string", params )

  /**
   * Makes the string uppercase
   * @return The string uppercased
   */
  override def replace() =
    string.sentStringValue.toUpperCase
}

/**
 * Makes the given string lowercase.
 * @param className The name of the class
 * @param params Params to the replacer
 * <ul><li>"string": The string to make lowercase</li></ul>
 * @author Kyle Dewey
 */
class StringMakeLowerCase( val className: String,
			   val params: Seq[ NamedParam ] ) extends Replacer {
  private val string = param( "string", params )

  /**
   * Makes the string lowercase
   * @return The string lowercased
   */
  override def replace() =
    string.sentStringValue.toLowerCase
}

/**
 * Capitalizes the given string.
 * For instance, if given "this is a string", this will return
 * "This Is A String".
 * @param className The name of the class
 * @param params Params to the replacer
 * <ul><li>"string": The string to capitalize</li></ul>
 * @author Kyle Dewey
 */
class Capitalize( val className: String,
		  val params: Seq[ NamedParam ] ) extends Replacer {
  private val string = param( "string", params )

  /**
   * Capitalizes the given string
   * @return The capitalized string
   */
  override def replace() = {
    var inString = false // true if we are in a string, as opposed to whitespace
    var retval = ""

    def push( item: Char ) {
      retval += "" + item
    }

    for{ x <- string.sentStringValue } {
      if ( Character.isWhitespace( x ) ) {
	inString = false
	push( x )
      } else {
	if ( !inString ) {
	  // first character after whitespace
	  inString = true
	  push( Character.toUpperCase( x ) )
	} else {
	  // in the string; don't change the character
	  push( x )
	}
      }
    }

    retval
  }
}

/**
 * Uses a regular expression to perform a substitution on the given string.
 * @param className The name of the class
 * @param params Parameters to the replacer
 * <ul><li>"string": The string to perform replacements on</li>
 * <li>"regex": The pattern to look for in the string</li>
 * <li>"replacement": The replacement for each instance of the pattern</li>
 * <li>"first": A matcher.  If it matches, then only the first item will
 * be replaced.  Otherwise, all items will be replaced.  Defaults to false.
 * </li><ul>
 * @author Kyle Dewey
 */
class Substitution( val className: String,
		    val params: Seq[ NamedParam ] ) extends Replacer {
  import java.util.regex._
  import sentinel.model.matcher._
  private val string = param( "string", params )
  private val regex = param( "regex" , params )
  private val replacement = param( "replacement", params )
  private val first = 
    opParam( "first", params ).getOrElse( new False ).matcherValue
  
  /**
   * Performs regex substitutions.
   * @return The original strings, with all instances of the regex pattern
   * replaced with the given replacement
   * @throws ReplaceException If the regex was invalid
   */
  override def replace() = {
    try {
      val compiled = regex.sentStringValue.r
      if ( !first.matches ) {
	// global replacements
	compiled.replaceAllIn( string.sentStringValue, 
			       replacement.sentStringValue )
      } else {
	// first replacement
	compiled.replaceFirstIn( string.sentStringValue,
				 replacement.sentStringValue )
      }
    } catch {
      case e: PatternSyntaxException =>
	throw new ReplaceException( e.toString )
    }
  }
}
