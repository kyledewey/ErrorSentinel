/*
 * Character.scala
 * 
 * Version:
 *     $Id: Character.scala,v 1.4 2011/06/08 04:25:53 kyledewey Exp $
 *
 * Revisions:
 *      $Log: Character.scala,v $
 *      Revision 1.4  2011/06/08 04:25:53  kyledewey
 *      Now conforms to the new Param interface.
 *
 *      Revision 1.3  2011/05/25 20:14:06  kyledewey
 *      Made it so the class name is also taken as a parameter.
 *
 *      Revision 1.2  2010/07/11 05:47:27  kyledewey
 *      Fixed bug in IsCharacter which made it always succeed
 *      if the input was a variable.
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
 * Holds helper routines for character
 * @author Kyle Dewey
 */
object Character {
  def toChars( param: Seq[ Param ] ) =
    param.map( ( p: Param ) =>
      new java.lang.Character( p.sentCharValue ) )
}

import Character._

/**
 * Matcher that will determine if the given item is a character or not
 * @param className The name of the class
 * @param params Params to the matcher
 *        <ul><li>"character": any data</li></ul>
 * @author Kyle Dewey
 */
class IsCharacter( val className: String,
		   val params: Seq[ NamedParam ] ) extends Matcher {
  private val character = param( "character", params )

  /**
   * Determines if the parameter is a character.
   * @return true if it is, else false
   */
  override def matches() = {
    try {
      character.sentCharValue
      true
    } catch {
      case e: ValueException => false
    }
  }
}

/**
 * Matcher that will determine if the given character is an
 * letter character.
 * @param className The name of the class
 * @param params Params to the matcher
 * <ul><li>"character": the character to test</li></ul>
 * @author Kyle Dewey
 */
class IsLetter( val className: String,
	        val params: Seq[ NamedParam ] ) extends Matcher {
  private val character = param( "character", params )

  /**
   * Determines if the given parameter is a letter.
   * this means it is something like a, b, c, etc.
   * @return true if it is, else false
   */
  override def matches() =
    java.lang.Character.isLetter( character.sentCharValue )
}

/**
 * Matcher that will determine if the given character is a
 * whitespace character.
 * @param className The name of the class
 * @param params Params to the matcher
 * <ul><li>"character": the character to test</li></ul>
 * @author Kyle Dewey
 */
class IsWhitespace( val className: String,
		    val params: Seq[ NamedParam ] ) extends Matcher {
  private val character = param( "character", params )

  /**
   * Determines if the given parameter is whitespace.
   * That means it's something like space, newline, tab, etc.
   * @return true if it is whitespace, else false
   */
  override def matches() =
    java.lang.Character.isWhitespace( character.sentCharValue )
}

/**
 * Matcher that determines whether or not the given character is
 * uppercase.
 * @param className The name of the class
 * @param params Params to the matcher
 * <ul><li>"character": the characetr to test</li></ul>
 * @author Kyle Dewey
 */
class IsUpperCase( val className: String,
		   val params: Seq[ NamedParam ] ) extends Matcher {
  private val character = param( "character", params )

  /**
   * Determines if the given parameter is uppercase.
   * That means it's something like A, B, C, etc.
   * @return True if it is upper case, else false
   */
  override def matches() =
    java.lang.Character.isUpperCase( character.sentCharValue )
}

/**
 * Matcher that determines whether or not the given character is
 * lowercase.
 * @param className The name of the class
 * @param params Params to the matcher
 * <ul><li>"character": the characetr to test</li></ul>
 * @author Kyle Dewey
 */
class IsLowerCase( val className: String,
		   val params: Seq[ NamedParam ] ) extends Matcher {
  private val character = param( "character", params )

  /**
   * Determines if the given parameter is lowercase.
   * That means it's something like A, B, C, etc.
   * @return True if it is upper case, else false
   */
  override def matches() =
    java.lang.Character.isLowerCase( character.sentCharValue )
}

/**
 * Matcher that determines whether or not the given character is
 * a digit.
 * @param className The name of the class
 * @param Params to the matcher
 * <ul><li>"character": the character to test</li></ul>
 * @author Kyle Dewey
 */
class IsDigit( val className: String,
	       val params: Seq[ NamedParam ] ) extends Matcher {
  private val character = param( "character", params )
  
  /**
   * Determines if the given parameter was a character.
   * That means it's something like 0, 1, 2, etc.
   * @return true if it is a digit, else false
   */
  override def matches() =
    java.lang.Character.isDigit( character.sentCharValue )
}

/**
 * Matcher that determines whether or not the given character is
 * alphanumeric.  That is, it is either a letter or a digit.
 * @param className The name of the class
 * @param Params to the matcher
 * <ul><li>"character": the character to test</li></ul>
 * @author Kyle Dewey
 */
class IsAlphaNumeric( val className: String,
		      val params: Seq[ NamedParam ] ) extends Matcher {
  private val character = param( "character", params )
  
  /**
   * Determines if the given parameter was alphanumeric.
   * That means it's something like 0, 1, 2, or a,b,c, etc.
   * @return true if it is alphanumeric, else false
   */
  override def matches() =
    java.lang.Character.isLetterOrDigit( character.sentCharValue )
}

/**
 * Matcher that determines if a list of characters are all equal.
 * @param className The name of the class
 * @param params Params to the matcher
 * <ul><li>"characters": The characters to compare</li></ul>
 * @author Kyle Dewey
 */
class CharEqual( val className: String,
		 val params: Seq[ NamedParam ] ) extends Matcher {
  private val characters = asArray( "characters", params )

  /**
   * Determines if the characters are all equal to each other.
   * @return true if they are, else false
   */
  override def matches() =
    Matcher.allEqual( toChars( characters ) )
}

/**
 * Matcher that determines if a list of characters are all not equal.
 * @param className The name of the class
 * @param params Params to the matcher
 * <ul><li>"characters": The characters to compare</li></ul>
 * @author Kyle Dewey
 */
class CharNotEqual( val className: String,
		    val params: Seq[ NamedParam ] ) extends Matcher {
  private val characters = asArray( "characters", params )

  /**
   * Determines if the characters are not equal to each other.
   * @return true if they are not, else false
   */
  override def matches() =
    Matcher.allDifferent( toChars( characters ) )
}

/**
 * Matcher that determines if a list of characters all are less than
 * each other.  For instance, a < b < c is true.
 * @param className The name of the class
 * @param params Params to the matcher
  <ul><li>"characters": The characters to compare</li></ul>
 * @author Kyle Dewey
 */
class CharLess( val className: String,
	        val params: Seq[ NamedParam ] ) extends Matcher {
  private val characters = asArray( "characters", params )

  /**
   * Determines if the characters are decreasing.
   * @return true if they are, else false
   */
  override def matches() =
    Matcher.allLess( toChars( characters ) )
}

/**
 * Matcher that determines if a list of characters all are greater than
 * each other.  For instance, c > b > a is true.
 * @param className The name of the class
 * @param params Params to the matcher
  <ul><li>"characters": The characters to compare</li></ul>
 * @author Kyle Dewey
 */
class CharGreater( val className: String,
		   val params: Seq[ NamedParam ] ) extends Matcher {
  private val characters = asArray( "characters", params )

  /**
   * Determines if the characters are increasing.
   * @return true if they are, else false.
   */
  override def matches() =
    Matcher.allGreater( toChars( characters ) )
}

class CharLessOrEqual( val className: String,
		       val params: Seq[ NamedParam ] ) extends Matcher {
  private val characters = asArray( "characters", params )
  override def matches() =
    Matcher.allLessOrEqual( toChars( characters ) )
}

class CharGreaterOrEqual( val className: String,
			  val params: Seq[ NamedParam ] ) extends Matcher {
  private val characters = asArray( "characters", params )
  override def matches() =
    Matcher.allGreaterOrEqual( toChars( characters ) )
}
