/*
 * GUIHelpers.scala
 *
 * Version:
 *     $Id: GUIHelpers.scala,v 1.1 2011/05/31 00:08:17 kyledewey Exp $
 *
 * Revisions:
 *      $Log: GUIHelpers.scala,v $
 *      Revision 1.1  2011/05/31 00:08:17  kyledewey
 *      Initial revision
 *
 *      Revision 1.2  2011/04/11 22:49:04  kyledewey
 *      Moved getParentJFrame's bulk to GUIHelpers.
 *
 *      Revision 1.1  2011/04/10 04:06:46  kyledewey
 *      Initial revision
 *
 *
 */

package sentinel.utils.interactive

import java.awt._
import javax.swing._
import javax.swing.table._

/**
 * Holds helper routines for GUI functions.
 * @author Kyle Dewey
 */
object GUIHelpers {
  /**
   * Like <code>makeComboBox</code>, but it uses <code>None</code>
   * for <code>selected</code>.
   * @param items The items to put in the box
   * @return A combo box holding the given items
   */
  def makeComboBox[ T <: java.lang.Object ]( items: Array[ T ] ): JComboBox[T] =
    makeComboBox( items, None )

  /**
   * Makes a combo box with the given items.
   * If there are <= 1 items in the box, the box isn't editable.
   * @param items The items to put in the box, in the order
   * they should be placed into the box
   * @param selected The item that should be selected.  Specify None for none.
   * @return a combo box holding the given items
   */
  def makeComboBox[ T <: java.lang.Object ]( items: Array[ T ],
				   selected: Option[ T ] ): JComboBox[T] = {
    val retval = 
      new JComboBox( items )

    if ( selected.isDefined ) {
      retval.setSelectedItem( selected.get ) 
    }

    if ( retval.getItemCount <= 1 ) {
      retval.setEditable( false )
      retval.setEnabled( false )
    }
    retval
  }

  /**
   * Makes a check box with the given initial value and set of
   * possible transformations.  If there is only one possible
   * transformation, the check box isn't enabled.
   * @param initial The initial value
   * @param transforms The possible transformations
   * @return A check box encapsulating the given information
   */
  def makeCheckBox( initial: Boolean, transforms: Set[ Boolean ] ) = {
    val retval = new JCheckBox( null, null, initial )

    if ( transforms.size <= 1 ) {
      retval.setEnabled( false )
    }
    retval
  }

  /**
   * Makes a parallel map of cell editors from the given combo boxes
   * @param combos The combo boxes
   * @return A parallel map of cell editors based on the combo boxes
   */
  def makeComboCellEditors[T]( combos: Seq[ JComboBox[T] ] ): Seq[ TableCellEditor ] =
    combos.map( new DefaultCellEditor( _ ) )

  /**
   * Makes a parallel map of cell editors from the given check boxes
   * @param checks The check boxes
   * @return A parallel map of cell editors based on the check boxes
   */
  def makeCheckCellEditors( checks: Seq[ JCheckBox ] ): Seq[ TableCellEditor ] =
    checks.map( new DefaultCellEditor( _ ) )

  /**
   * Makes a renderer for the given combo box
   * @param combo The combo box
   * @return A renderer for this combo box
   */
  def toComboRenderer[T]( combo: JComboBox[T] ): TableCellRenderer = {
    new TableCellRenderer() {
      /* http://www.exampledepot.com/egs/javax.swing.table/ComboBox.html */
      def getTableCellRendererComponent( table: JTable,
					 value: Object,
					 isSelected: Boolean,
					 hasFocus: Boolean,
					 row: Int,
					 column: Int ) = {
	if ( isSelected ) {
	  combo.setForeground( Color.BLACK )
	  combo.setBackground( table.getSelectionBackground )
	} else {
	  combo.setForeground( table.getForeground )
	  combo.setBackground( table.getBackground )
	}
	combo.setSelectedItem( value )
	combo
      }
    }
  }

  /**
   * Makes a renderer from the given component
   * Simply returns the component as is.
   * @param comp The component
   * @return A renderer for this component.
   */
  def defaultToRenderer( comp: Component ): TableCellRenderer =
    new TableCellRenderer() {
      def getTableCellRendererComponent( table: JTable,
					 value: Object,
					 isSelected: Boolean,
					 hasFocus: Boolean,
					 row: Int,
					 column: Int ) = comp
    }
  
  /**
   * Makes a renderer from the given component
   * @param comp The component
   * @return A renderer for this component
   */
  def toRenderer( comp: Component ): TableCellRenderer =
    comp match {
      case c: JComboBox[_] => toComboRenderer( c )
      case _ => defaultToRenderer( comp )
    }

  /**
   * Makes a parallel map of renderers from the given components.
   * @param comps The components
   * @return A parallel map of renderers
   */
  def renderers( comp: Seq[ Component ] ): Seq[TableCellRenderer] =
    comp.map(toRenderer)

  /**
   * Wraps an object into a Some/None.
   * @param item The object to wrap
   * @return The wrapped object
   */
  def toOption[ T ]( item: T ) =
    if ( item != null ) 
      Some( item )
    else None

  /**
   * Gets the parent JFrame that the given component is under.
   * @param comp The component container
   * @return The parent JFrame, if there is one.  Otherwise it
   * returns None.
   */
  final def getParentJFrame( comp: Container ): Option[ JFrame ] =
    if ( comp == null ) {
      None
    } else if ( comp.isInstanceOf[ JFrame ] ) {
      Some( comp.asInstanceOf[ JFrame ] )
    } else {
      getParentJFrame( comp.getParent )
    }
}
