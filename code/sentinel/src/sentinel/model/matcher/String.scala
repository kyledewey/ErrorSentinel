/*
 * String.scala
 * 
 * Version:
 *     $Id: String.scala,v 1.5 2011/06/08 04:25:53 kyledewey Exp $
 *
 * Revisions:
 *      $Log: String.scala,v $
 *      Revision 1.5  2011/06/08 04:25:53  kyledewey
 *      Now conforms to the new Param interface.
 *
 *      Revision 1.4  2011/06/02 06:56:13  kyledewey
 *      Added code so that the regex is only recompiled
 *      when the regex string actually changes, as opposed
 *      to each time the function is called.
 *
 *      Revision 1.3  2011/05/25 20:14:06  kyledewey
 *      Made it so the class name is also taken as a parameter.
 *
 *      Revision 1.2  2010/06/28 18:43:02  kyledewey
 *      Fixed typo.
 *
 *      Revision 1.1  2010/06/26 04:04:04  kyledewey
 *      Initial revision
 *
 *
 */

package sentinel.model.matcher

import sentinel.model._
import sentinel.model.InstanceFactory._

/**
 * Holds helper routines for String.
 * @author Kyle Dewey
 */
object StringHelpers {
  def toStrings( params: Seq[ Param ] ) =
    params.map( _.sentStringValue )
}

import StringHelpers._

/**
 * Determines if all the given strings are the same as each other.
 * @param className The name of the class
 * @param params Params to the matcher
 * <ul><li>"strings": The strings to test for equality</li></ul>
 * @author Kyle Dewey
 */
class StringEqual( val className: String,
		   val params: Seq[ NamedParam ] ) extends Matcher {
  private val strings = asArray( "strings", params )

  /**
   * Determines if all the given strings are equal to each other
   * @return true if they are all equal, else false
   */
  override def matches() =
    Matcher.allEqual( toStrings( strings ) )
}

/**
 * Determines if all the given strings are different from each other.
 * @param className The name of the class
 * @param params Params to the matcher
 * <ul><li>"strings": The strings to test for non-equality</li></ul>
 * @author Kyle Dewey
 */
class StringNotEqual( val className: String,
		      val params: Seq[ NamedParam ] ) extends Matcher {
  private val strings = asArray( "strings", params )

  /**
   * Determines if all the given strings are not equal to each other.
   * @return true if they are all different, else false
   */
  override def matches() =
    Matcher.allDifferent( toStrings( strings ) )
}

/**
 * Determines if all the given strings are monotonically increasing.
 * In other words, something like 1 > 2 > 3
 * @param className The name of the class
 * @param Params params to the matcher
 * <ul><li>"strings": The strings to test</li></ul>
 * @author Kyle Dewey
 */
class StringGreater( val className: String,
		     val params: Seq[ NamedParam ] ) extends Matcher {
  private val strings = asArray( "strings", params )

  /**
   * Determines if all the given strings are monotonically increasing
   * @return true if they are increasing, else false
   */
  override def matches() =
    Matcher.allGreater( toStrings( strings ) )
}

/**
 * Determines if all the given strings are monotonically decreasing.
 * In other words, something like 1 > 2 > 3
 * @param className The name of the class
 * @param Params params to the matcher
 * <ul><li>"strings": The strings to test</li></ul>
 * @author Kyle Dewey
 */
class StringLess( val className: String,
		  val params: Seq[ NamedParam ] ) extends Matcher {
  private val strings = asArray( "strings", params )

  /**
   * Determines if all the given strings are monotonically decreasing
   * @return true if they are decreasing, else false
   */
  override def matches() =
    Matcher.allLess( toStrings( strings ) )
}

class StringLessOrEqual( val className: String,
			 val params: Seq[ NamedParam ] ) extends Matcher {
  private val strings = asArray( "strings", params )
  override def matches() =
    Matcher.allLessOrEqual( toStrings( strings ) )
}

class StringGreaterOrEqual( val className: String,
			    val params: Seq[ NamedParam ] ) extends Matcher {
  private val strings = asArray( "strings", params )
  override def matches() =
    Matcher.allGreaterOrEqual( toStrings( strings ) )
}

/**
 * Matches a string against a regular expression.
 * @param className The name of the class
 * @param params Params to the matcher
 * <ul><li>"string": The string to try to find a regex match in</li>
 * <li>"regex": The regular expression to try to find in the string</li></ul>
 * @author Kyle Dewey
 */
class Regex( val className: String,
	     val params: Seq[ NamedParam ] ) extends Matcher {
  import java.util.regex.PatternSyntaxException
  import scala.util.matching.{ Regex => SRegex }
  private val string = param( "string", params )
  private val regexString = param( "regex", params )
  private var lastRegexString: Option[ String ] = None
  private var regex: Option[ SRegex ] = None

  /**
   * Determines if the string matches the regular expression.
   * @return true if it matches the pattern, else false
   * @throws MatchException If the regex string is invalid
   */
  override def matches() = {
    try {
      val currentRegexString = regexString.sentStringValue
      if ( lastRegexString.isEmpty ||
	   lastRegexString.get != currentRegexString ||
	   regex.isEmpty ) {
	lastRegexString = Some( currentRegexString )
	regex = Some( currentRegexString.r )
      }

      regex.get.findFirstIn( string.sentStringValue ).isDefined
    } catch {
      case e: PatternSyntaxException =>
	throw new MatchException( e.toString )
    }
  }
}
