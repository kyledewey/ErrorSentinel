/*
 * SentinelInformationPanel.scala
 */

package sentinel.utils.interactive

import java.awt._
import javax.swing._

/**
 * Panel that holds information pertaining to only a single table at
 * a time.  For example, there can be only one possible replacement
 * at a time, so this panel is used to show replacements.
 * @author Kyle Dewey
 */
object SentinelInformationPanel 
extends JInternalFrame( "Sentinel Information Panel", true, false, true, true ) {
  // begin instance variables
  private val info = makeInfoTextField
  add( info )
  // end instance variables

  /**
   * Makes the information text field.
   * @return The information text field
   */
  protected def makeInfoTextField() = {
    val retval = new JTextField( "" )
    retval.setBackground( Color.BLACK )
    retval
  }

  /**
   * Sets the info text to show the given text
   * with the given foreground color.
   * @param text The text to show
   * @param color The color of the text
   */
  def showText( text: String, color: Color ) {
    info.setText( text )
    info.setForeground( color )
  }
}
