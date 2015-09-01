/*
 * Matcher.scala
 *
 * Version:
 *     $Id: Matcher.scala,v 1.12 2011/06/18 03:30:08 kyledewey Exp $
 *
 * Revisions:
 *      $Log: Matcher.scala,v $
 *      Revision 1.12  2011/06/18 03:30:08  kyledewey
 *      Moved True and False to sentinel.model.Parameters.
 *
 *      Revision 1.11  2011/06/08 04:25:53  kyledewey
 *      Now conforms to the new Param interface.
 *
 *      Revision 1.10  2011/06/02 19:00:13  kyledewey
 *      Added the Not matcher.
 *
 *      Revision 1.9  2011/06/02 00:20:00  kyledewey
 *      The base case for forallPairs that
 *      takes a single value now dereferences that value
 *      as a no-op before returning true.  This forces
 *      a ValueException to be generated on invalid data.
 *
 *      Revision 1.8  2011/05/25 20:14:06  kyledewey
 *      Made it so the class name is also taken as a parameter.
 *
 *      Revision 1.7  2010/06/26 16:35:43  kyledewey
 *      Added no argument constructors for true and false,
 *      intended as convenience for built-ins.
 *
 *      Revision 1.6  2010/06/26 04:04:04  kyledewey
 *      Added helper functions common to many matchers.
 *
 *      Revision 1.5  2010/06/20 17:29:04  kyledewey
 *      Moved factory code to sentinel.model.Factory.scala
 *
 *      Revision 1.4  2010/06/18 19:36:41  kyledewey
 *      Made factories take a name and description.
 *
 *      Revision 1.3  2010/06/18 03:07:47  kyledewey
 *      Made it so the factory calls internalInstantiate()
 *
 *      Revision 1.2  2010/06/16 01:00:12  kyledewey
 *      Fixed typo in name of ParameterizedInstantiationException.
 *
 *      Revision 1.1  2010/06/15 17:55:00  kyledewey
 *      Initial revision
 *
 *
 */

package sentinel.model.matcher

import sentinel.model._
import sentinel.model.InstanceFactory._

/**
 * Contains routines that are useful for matchers.
 * @author Kyle Dewey
 */
object Matcher {
  /**
   * Executes a given predicate for each pair of items.
   * Note that this means a comparison of (1,2),(2,3), etc., and
   * is still O( n )
   * @param items The items to look at
   * @param pred The predicate to test with
   * @return true if the predicate matches on all pairs as described
   * @throws ValueException If we could not get the value of a given element
   */
  def forallPairs[ T ]( items: Seq[ T ] )( pred: ( T, T ) => Boolean ) = {
    def forPairs( list: List[ T ] ): Boolean = 
      list match {
	case Nil => true
	case item :: Nil => true
	case head :: middle :: tail => {
	  if ( pred( head, middle ) ) {
	    forPairs( middle :: tail )
	  } else false
	}
      }
    forPairs( items.toList )
  }

  /**
   * Compares all items to make sure that each is less than the last
   * @param items The items to compare
   * @return true if item1 < item2 < item3, else false
   * @throws ValueException if we could not get the value of any single element
   */
  def allLess[ T <: Comparable[ T ] ]( items: Seq[ T ] ) =
    forallPairs( items )( _.compareTo( _ ) < 0 )

  /**
   * Compares all items to make sure that each is greater than the last
   * @param items The items to compare
   * @return true if item1 > item2 > item3
   * @throws ValueException If we could not get the value of any single element
   */
  def allGreater[ T <: Comparable[ T ] ]( items: Seq[ T ] ) =
    forallPairs( items )( _.compareTo( _ ) > 0 )

  /**
   * Compares all items to see if they are monotonically increasing.
   * In other words, 3 <= 4 <= 4.
   * @param items The items to compare
   * @return true if the items are monotonically increasing
   * @throws ValueException If we could not get the value of any single element
   */
  def allLessOrEqual[ T <: Comparable[ T ] ]( items: Seq[ T ] ) =
    forallPairs( items )( _.compareTo( _ ) <= 0 )

  /**
   * Compares all items to see if they are monotonically decreasing.
   * In other words, 4 >= 3 >= 2
   * @param items The items to compare
   * @return true if the items are monotonically decreasing
   * @throws ValueException If we could not get the value of any single element
   */
  def allGreaterOrEqual[ T <: Comparable[ T ] ]( items: Seq[ T ] ) =
    forallPairs( items )( _.compareTo( _ ) >= 0 )
  
  /**
   * Compares all items to make sure that they all have the same value.
   * @param items The items to compare
   * @return true if they all had the same value, else false
   * @throws ValueException If we could not get the value out of any element
   * for whatever reason
   */
  def allEqual[ T ]( items: Seq[ T ] ) = 
    forallPairs( items )( _ == _ )

  /**
   * Compares all items to make sure that they all have different
   * values.
   * @param items The items to compare
   * @return true if they all had different values, else false
   * @throws ValueException If we could not get the value out of any element
   */
  def allDifferent[ T ]( items: Seq[ T ] ) = {
    val asSet = Set() ++ items
    asSet.size == items.size
  }
}

/**
 * Matcher that will invert the given matcher's return value
 * @param className The name of the class
 * @param params The params to the matcher
 * <ul><li>"matcher" - matcher to invert the value of</li></ul>
 * @author Kyle Dewey
 */
class Not( val className: String,
	   val params: Seq[ NamedParam ] ) extends Matcher {
  private val input = param( "matcher", params ).matcherValue

  /**
   * Merely returns the opposite of the given matcher
   * @return The opposite of the given matcher
   */
  override def matches() =
    !input.matches
}

/**
 * Matcher that represents a logical and.
 * @param className The name of the class
 * @param params the params to the matcher
 *        <ul><li>"input" - 0 or more matchers</li></ul>
 * @author Kyle Dewey
 */
class And( val className: String, 
	   val params: Seq[ NamedParam ] ) extends Matcher {
  private val input = InstanceFactory.matchers( asArray( "input", params ) )

  /**
   * Returns true if and only if all given matchers return true.
   * Supports short circuit evaluation.  If no matchers are given,
   * it returns true.
   * @return true if all matchers returned true, else false
   */
  override def matches(): Boolean = 
    // note that forall short circuits
    input.forall( _.matches )
}

/**
 * Matcher that represents a logical or.
 * @param className The name of the class
 * @param params Params to the matcher
 *        <ul><li>"input" - 0 or more matchers</li></ul>
 * @author Kyle Dewey
 */
class Or( val className: String, 
	  val params: Seq[ NamedParam ] ) extends Matcher {
  private val input = InstanceFactory.matchers( asArray( "input", params ) )
  
  /**
   * Returns false if and only if all matchers return false.
   * Supports short circuit evaluation  If no matchers are
   * given, it returns false
   * @return false if all matchers returned false, else true
   */
  override def matches(): Boolean = 
    // note that exists short circuits
    input.exists( _.matches )
}

/**
 * Matcher that will match something exactly
 * @param className The name of the class
 * @param params Params to the matcher<ul>
 *        <li>"input": any data</li>
 *        <li>"against": any data</li></ul>
 * @author Kyle Dewey
 */
class Exact( val className: String, 
	     val params: Seq[ NamedParam ] ) extends Matcher {
  private val input = param( "input", params )
  private val against = param( "against", params )

  /**
   * Returns true if the two params are of the same value
   * @return true if equal, else false
   */
  override def matches() =
    input.sentStringValue == against.sentStringValue
}

/**
 * Merely throws a matcher exception.
 * @param className The name of the class
 * @param params Params to the matcher
 * <ul><li>"message": any data to show what happened</li></ul>
 * @author Kyle Dewey
 */
class ThrowMatchException( val className: String, 
			   val params: Seq[ NamedParam ] ) extends Matcher {
  private val message = param( "message", params )

  /**
   * Doesn't actually ever return.
   * Merely throws an exception with whatever message was given.
   * @return nothing
   * @throws MatchException Under all cases
   */
  override def matches() =
    throw new MatchException( message.sentStringValue )
}
