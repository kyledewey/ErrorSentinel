/*
 * InstanceResult.scala
 * 
 * Version:
 *     $Id: InstanceResult.scala,v 1.2 2011/06/08 04:23:43 kyledewey Exp $
 *
 * Revisions:
 *      $Log&
 *
 */

package sentinel.model

/**
 * Contains helper routines for Instance Results.
 * @author Kyle Dewey
 */
object InstanceResult {
  /**
   * Executes the given matcher, and returns an InstanceResult
   * describing the execution.
   * @param matcher The matcher to execute
   * @return An InstanceResult describing what happened
   */
  def executeMatcher( matcher: Matcher ) =
    try {
      if ( matcher.matches() ) new InstanceSuccess()
      else new MatcherFailure( matcher )
    } catch {
      case e: Exception => new InstanceFailureException( matcher, e )
    }

  /**
   * Executes the given replacer, and returns an InstanceResult
   * describing the execution.
   * @param replacer The replacer o execute
   * @return An InstanceResult describing what happened
   */
  def executeReplacer( replacer: Replacer ) =
    try {
      new InstanceFailureReplacement( replacer,
				      replacer.sentStringValue )
    } catch {
      case e: Exception => new InstanceFailureException( replacer, e )
    }

  /**
   * Executes the given matcher/replacer pair, and returns an
   * InstanceResult describing the execution of the replacer.
   * @param pair The matcher/replacer pair
   * @return The InstanceResult of the replacer, or None if the matcher
   * didn't match (either returned false or threw an exception)
   */
  def executeErrorCorrectionPair( pair: Pair[ Matcher, Replacer ] ) =
    if ( executeMatcher( pair._1 ).success ) 
      Some( executeReplacer( pair._2 ) )
    else None

  /**
   * Determines what the given instance result is, and executes
   * specific code for it.
   * @param instanceResult The instance result
   * @param success Function to call on success.
   * @param failureReplacement Function to call when there was an error with
   * an autoreplacement
   * @param failureException Function to call when there was an error and
   * a matching autocorrect rule threw an exception
   * @param totalFailure Function to call when the data was considered invalid
   * and there were no matching autocorrect rules.
   */
  def dispatchOnInstanceResult( instanceResult: Option[ InstanceResult ],
			        success: InstanceSuccess => Unit,
			        failureReplacement: InstanceFailureReplacement => Unit,
			        failureException: InstanceFailureException[ _ ] => Unit,
			        totalFailure: () => Unit ) {
    if ( instanceResult.isDefined ) {
      instanceResult.get match {
	case s: InstanceSuccess => success( s )
	case r: InstanceFailureReplacement => failureReplacement( r )
	case e: InstanceFailureException[ _ ] => failureException( e )
      }
    } else {
      totalFailure()
    }
  }
}
      
/**
 * Base class for instance results.
 * @author Kyle Dewey
 */
sealed trait InstanceResult {
  /**
   * Indicates failure occurred.
   * @return if this failed
   */
  def failure(): Boolean

  /**
   * Indicates success occurred.
   * @return if this succeeded
   */
  def success() = !failure
}

/**
 * Instance result indicating success.
 * This means that the input value is considered valid.
 * @author Kyle Dewey
 */
class InstanceSuccess extends InstanceResult {
  /**
   * Returns false
   * @return <code>false</code>
   */
  def failure() = false
}

/**
 * Instance result indicating failure.
 * Note that there are different ways in which it could have failed, which
 * are relevant to matchers and replacers.
 * @param failedInstance The instance that it failed on
 * @author Kyle Dewey 
 */
abstract class InstanceFailure[ T <: Instance ]( val failedInstance: T ) 
extends InstanceResult {
  /**
   * Returns true
   * @return <code>true</code>
   */
  def failure() = true

  /**
   * Returns true if an exception was thrown.
   * @return true if an exception was thrown
   */
  def exceptionThrown(): Boolean
}

/**
 * Instance result for a matcher that returned false.
 * @param failedMatcher The matcher that failed
 * @author Kyle Dewey
 */
class MatcherFailure( failedMatcher: Matcher ) 
extends InstanceFailure[ Matcher ]( failedMatcher ) {
  /**
   * Returns false.
   * @return <code>false</code>
   */
  def exceptionThrown() = false
}

/**
 * Instance result indicating failure due to an exception being thrown
 * @param failedInstance The instance that failed
 * @param exception The exception that was thrown
 * @author Kyle Dewey
 */
class InstanceFailureException[ T <: Instance ]( failedInstance: T,
						 val exception: Exception )
extends InstanceFailure[ T ]( failedInstance ) {
  /**
   * Returns true.
   * @return <code>true</code>
   */
  def exceptionThrown() = true
}

/**
 * Instance result indicating failure, but with a replacement to show for it.
 * @param failedInstance The instance that failed
 * @param replacement The replacement
 * @author Kyle Dewey
 */
class InstanceFailureReplacement( failedInstance: Replacer,
				  val replacement: String ) 
extends InstanceFailure[ Replacer ]( failedInstance ) {
  /**
   * Returns false.
   * @return <code>false</code>
   */
  def exceptionThrown() = false
}
