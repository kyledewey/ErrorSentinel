/*
 * SentinelTablePanel.scala
 *
 * Version:
 *     $Id: SentinelTablePanel.scala,v 1.4 2011/05/30 04:03:25 kyledewey Exp $
 *
 * Revisions:
 *      $Log: SentinelTablePanel.scala,v $
 *      Revision 1.4  2011/05/30 04:03:25  kyledewey
 *      Added a name() convenience method.
 *
 *      Revision 1.3  2011/05/28 02:41:38  kyledewey
 *      Frames are now used directly, with no panel intermediary.
 *      This allows for correct resizing behavior of the JTable.
 *
 *      Revision 1.2  2011/05/27 18:52:09  kyledewey
 *      Made it so panels can be resized, maximized,
 *      and minimized.
 *
 *      Revision 1.1  2011/05/27 01:37:30  kyledewey
 *      Initial revision
 *
 *
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
