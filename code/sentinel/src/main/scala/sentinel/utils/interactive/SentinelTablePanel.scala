/*
 * SentinelTablePanel.scala
 */

package sentinel.utils.interactive

import java.awt._
import javax.swing._

/**
 * An internal frame that holds a sentinel table.
 * Note that for table resizing to work properly, this must be directly
 * in the panel.
 * @param panel The panel holding the table
 * @author Kyle Dewey
 */
class SentinelTableFrame( val table: SentinelTable )
extends JInternalFrame( table.name, true, false, true, true ) {
  // begin constructor
  add( new JScrollPane( table ) )
  // end constructor

  /**
   * Gets the name of the underlying table.
   * @return <code>table.name</code>
   */
  def name() =
    table.name
}
