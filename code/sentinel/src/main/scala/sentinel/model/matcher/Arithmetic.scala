/*
 * Arithmetic.scala
 *
 * Version:
 *     $Id: Arithmetic.scala,v 1.3 2011/06/08 04:25:53 kyledewey Exp $
 *
 * Revisions:
 *      $Log: Arithmetic.scala,v $
 *      Revision 1.3  2011/06/08 04:25:53  kyledewey
 *      Now conforms to the new Param interface.
 *
 *      Revision 1.2  2011/05/25 20:14:06  kyledewey
 *      Made it so the class name is also taken as a parameter.
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
 * Holds helper functions for arithmetic routines.
 * @author Kyle Dewey
 */
object Arithmetic {
  def toInts( params: Seq[ Param ] ) =
    params.map( ( p: Param ) =>
      new java.lang.Long( p.sentIntValue ) )
  def toReals( params: Seq[ Param ] ) =
    params.map( ( p: Param ) =>
      new java.lang.Double( p.sentRealValue ) )
}

import Arithmetic._

/**
 * Determines if all the given integers are the same as each other.
 * @param className The name of the class
 * @param params Params to the matcher
 * <ul><li>"integers": The integers to test for equality</li></ul>
 * @author Kyle Dewey
 */
class IntEqual( val className: String,
	        val params: Seq[ NamedParam ] ) extends Matcher {
  private val integers = asArray( "integers", params )

  /**
   * Determines if all the given integers are equal to each other
   * @return true if they are all equal, else false
   */
  override def matches() =
    Matcher.allEqual( toInts( integers ) )
}

/**
 * Determines if all the given integers are different from each other.
 * @param className The name of the class
 * @param params Params to the matcher
 * <ul><li>"integers": The integers to test for non-equality</li></ul>
 * @author Kyle Dewey
 */
class IntNotEqual( val className: String,
		   val params: Seq[ NamedParam ] ) extends Matcher {
  private val integers = asArray( "integers", params )

  /**
   * Determines if all the given integers are not equal to each other.
   * @return true if they are all different, else false
   */
  override def matches() =
    Matcher.allDifferent( toInts( integers ) )
}

/**
 * Determines if all the given integers are monotonically increasing.
 * In other words, something like 1 > 2 > 3
 * @param className The name of the class
 * @param Params params to the matcher
 * <ul><li>"integers": The integers to test</li></ul>
 * @author Kyle Dewey
 */
class IntGreater( val className: String,
		  val params: Seq[ NamedParam ] ) extends Matcher {
  private val integers = asArray( "integers", params )

  /**
   * Determines if all the given integers are monotonically increasing
   * @return true if they are increasing, else false
   */
  override def matches() =
    Matcher.allGreater( toInts( integers ) )
}

/**
 * Determines if all the given integers are monotonically decreasing.
 * In other words, something like 1 > 2 > 3
 * @param className The name of the class
 * @param Params params to the matcher
 * <ul><li>"integers": The integers to test</li></ul>
 * @author Kyle Dewey
 */
class IntLess( val className: String,
	       val params: Seq[ NamedParam ] ) extends Matcher {
  private val integers = asArray( "integers", params )

  /**
   * Determines if all the given integers are monotonically decreasing
   * @return true if they are decreasing, else false
   */
  override def matches() =
    Matcher.allLess( toInts( integers ) )
}

class IntLessOrEqual( val className: String,
		      val params: Seq[ NamedParam ] ) extends Matcher {
  private val integers = asArray( "integers", params )
  override def matches() =
    Matcher.allLessOrEqual( toInts( integers ) )
}

class IntGreaterOrEqual( val className: String,
			 val params: Seq[ NamedParam ] ) extends Matcher {
  private val integers = asArray( "integers", params )
  override def matches() =
    Matcher.allGreaterOrEqual( toInts( integers ) )
}

/**
 * Determines if all the given reals are the same as each other.
 * @param className The name of the class
 * @param params Params to the matcher
 * <ul><li>"reals": The reals to test for equality</li></ul>
 * @author Kyle Dewey
 */
class RealEqual( val className: String,
		 val params: Seq[ NamedParam ] ) extends Matcher {
  private val reals = asArray( "reals", params )

  /**
   * Determines if all the given reals are equal to each other
   * @return true if they are all equal, else false
   */
  override def matches() =
    Matcher.allEqual( toReals( reals ) )
}

/**
 * Determines if all the given reals are different from each other.
 * @param className The name of the class
 * @param params Params to the matcher
 * <ul><li>"reals": The reals to test for non-equality</li></ul>
 * @author Kyle Dewey
 */
class RealNotEqual( val className: String,
		    val params: Seq[ NamedParam ] ) extends Matcher {
  private val reals = asArray( "reals", params )

  /**
   * Determines if all the given reals are not equal to each other.
   * @return true if they are all different, else false
   */
  override def matches() =
    Matcher.allDifferent( toReals( reals ) )
}

/**
 * Determines if all the given reals are monotonically increasing.
 * In other words, something like 1 > 2 > 3
 * @param className The name of the class
 * @param Params params to the matcher
 * <ul><li>"reals": The reals to test</li></ul>
 * @author Kyle Dewey
 */
class RealGreater( val className: String,
		   val params: Seq[ NamedParam ] ) extends Matcher {
  private val reals = asArray( "reals", params )

  /**
   * Determines if all the given reals are monotonically increasing
   * @return true if they are increasing, else false
   */
  override def matches() =
    Matcher.allGreater( toReals( reals ) )
}

/**
 * Determines if all the given reals are monotonically decreasing.
 * In other words, something like 1 > 2 > 3
 * @param className The name of the class
 * @param Params params to the matcher
 * <ul><li>"reals": The reals to test</li></ul>
 * @author Kyle Dewey
 */
class RealLess( val className: String,
	        val params: Seq[ NamedParam ] ) extends Matcher {
  private val reals = asArray( "reals", params )

  /**
   * Determines if all the given reals are monotonically decreasing
   * @return true if they are decreasing, else false
   */
  override def matches() =
    Matcher.allLess( toReals( reals ) )
}

class RealLessOrEqual( val className: String,
		       val params: Seq[ NamedParam ] ) extends Matcher {
  private val reals = asArray( "reals", params )
  override def matches() =
    Matcher.allLessOrEqual( toReals( reals ) )
}

class RealGreaterOrEqual( val className: String,
			  val params: Seq[ NamedParam ] ) extends Matcher {
  private val reals = asArray( "reals", params )
  override def matches() =
    Matcher.allGreaterOrEqual( toReals( reals ) )
}
