/*
 * Character.scala
 *
 * Version:
 *     $Id: Character.scala,v 1.4 2011/06/08 04:26:15 kyledewey Exp $
 *
 * Revisions:
 *      $Log: Character.scala,v $
 *      Revision 1.4  2011/06/08 04:26:15  kyledewey
 *      Now conforms to the new Param interface.
 *
 *      Revision 1.3  2011/06/07 08:17:44  kyledewey
 *      Added replacer types to nonpolymorphic routines.
 *
 *      Revision 1.2  2011/05/25 20:14:58  kyledewey
 *      Made it so the class name is also taken as a parameter.
 *
 *      Revision 1.1  2010/06/26 04:05:40  kyledewey
 *      Initial revision
 *
 *
 */

package sentinel.model.replacer

import sentinel.model._
import sentinel.model.matcher.Character._
import sentinel.model.Replacer._
import sentinel.model.InstanceFactory._

/**
 * Replacer that will make the given character uppercase.
 * @param className The name of the class
 * @param params Parameters for the replacer
 * <ul><li>"character": The character to make uppercase</li></ul>
 * @author Kyle Dewey
 */
class CharMakeUpperCase( val className: String,
			 val params: Seq[ NamedParam ] ) extends Replacer {
  private val character = param( "character", params )

  /**
   * Makes the given character uppercase.
   * @return The given character, uppercased
   */
  override def replace() =
    java.lang.Character.toUpperCase( character.sentCharValue )
}

/**
 * Replacer that will make the given character lowercase.
 * @param className The name of the class
 * @param params Parameters for the replacer
 * <ul><li>"character": The character to make lowercase</li></ul>
 * @author Kyle Dewey
 */
class CharMakeLowerCase( val className: String,
			 val params: Seq[ NamedParam ] ) extends Replacer {
  private val character = param( "character", params )

  /**
   * Makes the given character lowercase.
   * @return The given character, lowercased
   */
  override def replace() =
    java.lang.Character.toLowerCase( character.sentCharValue )
}

class CharMax( val className: String,
	       val params: Seq[ NamedParam ] ) extends Replacer {
  private val chars = asArray( "chars", params )
  override def replace() =
    Replacer.max( toChars( chars ) ).charValue
}

class CharMin( val className: String,
	       val params: Seq[ NamedParam ] ) extends Replacer {
  private val chars = asArray( "chars", params )
  override def replace() =
    Replacer.min( toChars( chars ) ).charValue
}
