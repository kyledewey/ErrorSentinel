/*
 * Description.scala
 */

package sentinel.model

/**
 * Exception thrown when the given input name is unrecognized.
 * @param message A message to show the user
 * @author Kyle Dewey
 */
case class UnknownInputException( message: String ) 
     extends Exception( message ) {}

/**
 * Holds constants for Description
 * @author Kyle Dewey
 */
object Description {
  // default header information
  val DEFAULT_HEADER = Array( "Name", "Description" )
}

/**
 * Trait for something that can be described by the description panel.
 * Note that arrays are used widely for rapid random access.
 * @author Kyle Dewey
 */
trait Description {
  /**
   * Gets the name of the thing being described
   * @return The name of the thing being described.
   */
  def name(): String

  /**
   * Gets a description of the item.
   * @return A description of the item
   */
  def description(): String

  /**
   * Gets header information for the inputs to this item.
   * By default, this returns <code>DEFAULT_HEADER</code>
   * @return header information for this item's inputs
   */
  def inputHeader(): Array[ String ] =
    Description.DEFAULT_HEADER

  /**
   * Gets the name of each input, in the desired order that they are
   * to be displayed.
   * @return The name of each input, in the desired order
   */
  def inputNames(): Array[ String ]

  /**
   * Gets a description of the given input.
   * @param input The name of the input
   * @return A description of the given input
   * @throws UnknownInputException If the given input name isn't
   * recognized
   */
  def inputDescription( input: String ): String

  /**
   * Gets a detailed description of the given input.
   * Assumes that the number of elements in this line is the same as
   * the number of elements returned by <code>inputHeader</code>, and that
   * the descriptions from <code>inputHeader</code> match what this returns.
   * @param input The name of the input to get a detailed description of
   * @return A detailed sequence of information describing the given input.
   * By default, this returns the name and description of the input
   * @throws UnknownInputException If the given input name isn't
   * recognized
   */
  def detailedDescription( input: String ): Array[ String ] =
    Array( input, inputDescription( input ) )

  /**
   * Gets a detailed description of all inputs.
   * The input descriptions are returned in the order specified by
   * <code>inputNames()</code>.
   * @return A detailed description of all inputs
   */
  def detailedDescription(): Array[ Array[ String ] ] =
    inputNames.map( detailedDescription( _ ) ).toArray

  /**
   * Gets a description of the output
   * @return A description of the output
   */
  def outputDescription(): String
}

/**
 * Holds constants for SentinelDescription.
 * @author Kyle Dewey
 */
object SentinelDescription {
  // sentinel-specific header
  val HEADER =
    Array( "Name", "Description", "Type", "Array?", "Required?" )
}

/**
 * Specifically for a description of a component in sentinel.
 * @author Kyle Dewey
 */
trait SentinelDescription extends Description {
  /**
   * Merely gets the sentinel-specific header
   * @return <code>SentinelDescription.HEADER</code>
   */
  override def inputHeader() =
    SentinelDescription.HEADER

  /**
   * Sentinel doesn't describe outputs, so this merely returns
   * a null string.
   * @return a null string
   */
  def outputDescription() = ""
}

/**
 * Trait that desribes something that one can get a describer for it.
 * @author Kyle Dewey
 */
trait Describable {
  /**
   * Gets the thing that can describe the given item.
   * @return The describer for this
   */
  def describer(): Description
}
