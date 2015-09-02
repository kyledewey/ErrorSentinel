/*
 * XMLHelpers.scala
 */

package sentinel.model

import scala.xml._

/**
 * Contains helper routines for dealing with XML.
 * @author Kyle Dewey 
 */
object XMLHelpers {
  // begin constants
  val DEFAULT_EXPECTED_NUM = 1
  // end constants

  /**
   * Given a node and the name of child nodes to look for, returns
   * the child nodes.  Verifies that the number of child nodes is the
   * same as what was expected.
   * @param node The root node to look under
   * @param tag The tag of interest
   * @param expectedNum The number of expected child nodes
   * @param exceptionMaker Makes exceptions in case an error occurs
   * @return Child nodes
   * @throws ClassParseException If the number of expected child nodes differs
   */
  def getNodes[ T <: Exception ]( node: Node, tag: String, expectedNum: Int, exceptionMaker: String => T ) = {
    val children = node \ tag
    val size = children.size
    if ( children.size != expectedNum ) {
      throw exceptionMaker( "Exception " + expectedNum + " tags under " +
			    node.label + " with tag \"" + tag +
			    "\"; found " + size )
    }
    children
  }

  /**
   * Like getNodes, but with the expected num as DEFAULT_EXPECTED_NUM.
   * @param node The root node to look under
   * @param tag The tag of interest
   * @param exceptionMaker Makes exceptions in case an error occurs
   * @return Child nodes
   * @throws ClassParseException If the number of expected child nodes differs
   *         from DEFAULT_EXPECTED_NUM
   */
  def getNodes[ T <: Exception ]( node: Node, tag: String, exceptionMaker: String => T ): NodeSeq = 
    getNodes( node, tag, DEFAULT_EXPECTED_NUM, exceptionMaker )

  /**
   * Given a node and the name of child nodes to look for, returns
   * the text of the child nodes concatentated together.
   * If the expected number of nodes differs from the number found,
   * it throws an exception
   * @param node The root node to look under
   * @param tag The tag of interest
   * @param expectedNum The number of expected child nodes
   * @param exceptionMaker Makes exceptions in case an error occurs
   * @return The text of all the child nodes concatenated together
   * @throws ClassParseException If the number of expected child nodes differs
   */
  def getText[ T <: Exception ]( node: Node, tag: String, expectedNum: Int, exceptionMaker: String => T ): String = 
    getNodes( node, tag, expectedNum, exceptionMaker ).text

  /**
   * Like getText, but with an expectedNum of
   * DEFAULT_EXPECTED_NUM
   * @param node The root node to look under
   * @param tag The tag of interest
   * @param exceptionMaker Makes exceptions in case an error occurs
   * @return The text of all child nodes concatenated together
   * @throws ClassParseException If there were no tages by the
   *         given name, or if the number of child nodes differs
   *         from DEFAULT_EXPECTED_NUM
   */
  def getText[ T <: Exception ]( node: Node, tag: String, exceptionMaker: String => T ): String =
    getText( node, tag, DEFAULT_EXPECTED_NUM, exceptionMaker )

  /**
   * Parses in text from the given base node with the given child
   * node name.  If the child node exists, it will return the text
   * contained in the child node.  Otherwise it returns None.
   * @param base The base node
   * @param childName The name of the child node
   * @return The text held in the child node, or None if there isn't
   * a child node.
   */
  def getOptionalText( base: Node, childName: String ) = {
    val child = base \ childName
    if ( child.isEmpty ) {
      None
    } else {
      Some( child.text )
    }
  }
}
