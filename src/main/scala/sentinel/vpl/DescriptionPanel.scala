/*
 * DescriptionPanel.scala
 */

package sentinel.vpl

import sentinel.model._
import javax.swing._
import javax.swing.event._
import javax.swing.table._
import java.awt._
import java.awt.event._

/**
 * Special table model that is intended to be able to quickly completely
 * change the data that the table shows
 * @param data The data to put into the table
 * @author Kyle Dewey
 */
class ReplaceTableModel( private var data: Array[ Array[ String ] ] ) 
extends AbstractTableModel {
  /**
   * Gets the number of rows in the table
   * @return The number of rows in the table
   */
  def getRowCount() =
    data.length

  /**
   * Gets the number of columns in the table
   * @return The number of columns in the table
   */
  def getColumnCount() =
    if ( data.length == 0 ) 0 
    else data( 0 ).length
  
  /**
   * Gets the value at the given row and column of the table
   * @param row The row of the table
   * @param column The column of the table
   * @return The item at this row and column
   */
  def getValueAt( row: Int, column: Int ) =
    data( row )( column )

  /**
   * Changes the data in the model.  Updates reports that a change occurred.
   * @param newData The new data to put into the table
   */
  def changeData( newData: Array[ Array[ String ] ] ) {
    data = newData
    fireTableStructureChanged()
  }
}

/**
 * Holds constants for <code>DescriptionPanelTableModel</code>
 * @author Kyle Dewey
 */
object DescriptionPanelTableModel {
  // default starting data
  val DEFAULT_DATA: Array[ Array[ String ] ] =
    Array.ofDim( 1, ParameterTableModel.COLUMN_NAMES.length - 1 )
  0.until( ParameterTableModel.COLUMN_NAMES.length - 1 ).foreach( index =>
    DEFAULT_DATA( 0 )( index ) = "" )
}

/**
 * The model for the table in the description panel.
 * @author Kyle Dewey
 */
class DescriptionPanelTableModel 
extends ReplaceTableModel( DescriptionPanelTableModel.DEFAULT_DATA ) {
  /**
   * Gets the name of the given column.
   * The names are taken from ParameterTableModel.COLUMN_NAMES
   * @param column The column
   * @return The name of the given column
   */
  override def getColumnName( column: Int ) =
    ParameterTableModel.COLUMN_NAMES( column ).toString
}

/**
 * A table that appears editable but is otherwise immutable.
 * This kind of table allows for all the text in a cell to be selected
 * and even appear to be changed, but none of the edits commit.
 * @author Kyle Dewey
 */
trait PseudoMutableTable extends JTable {
  /**
   * Overridden to do nothing.
   * @param value The value to set (ignored)
   * @param row The row (ignored)
   * @param column The column (ignored)
   */
  override def setValueAt( value: Object, row: Int, column: Int ) {}

  /**
   * Overridden to return true.
   * @param row The row of the cell (ignored)
   * @param column The column of the cell (ignored)
   */
  override def isCellEditable( row: Int, column: Int ) = true
}

/**
 * The JTable for the description panel.
 * This is used to show information about selected components.
 * @param model The underlying model
 * @author Kyle Dewey
 */
class DescriptionPanelTable( val model: ReplaceTableModel ) 
extends JTable( model ) with PseudoMutableTable {
  // begin constructor
  setGridColor( Color.BLACK )
  setShowGrid( true )
  setShowHorizontalLines( true )
  setShowVerticalLines( true )
  setAlignmentX( 0.0f )
  // end constructor

  /**
   * Gets the table header.
   * Merely calls super to do this, and sets the resulting header
   * to have an X alignment of 0.0f.
   * @return A table header meeting the above criteria
   */
  override def getTableHeader() = {
    val retval = super.getTableHeader
    retval.setAlignmentX( 0.0f )
    retval
  }
}

/**
 * Holds constants for the description panel.
 * @author Kyle Dewey
 */
object DescriptionPanel {
  // prefix for the output description
  val OUTPUT_DESC_PREFIX = "Output: "
}

/**
 * Represents the description panel in the VPL.  The description panel describes
 * all inputs and outputs to the selected component.
 * @param gui The gui that is holding everything together
 * @author Kyle Dewey
 */
class DescriptionPanel extends JPanel {
  val name = createLabel( "Name" ) 
  name.setAlignmentX( 0.0f )
  val desc = createDescription( "Description" )
  val tableModel = new DescriptionPanelTableModel()
  val table = new DescriptionPanelTable( tableModel )
  val outputLabel = createLabel( "Output Description" )
  outputLabel.setAlignmentX( 0.0f )

  setLayout( new BoxLayout( this, BoxLayout.Y_AXIS ) )
  add( name )
  add( desc )
  add( createLabel( "Inputs:" ) )
  add( table.getTableHeader )
  add( table )
  add( outputLabel )

  /**
   * Creates a generic label with the given text.
   * @param text The text to put into the label
   * @return The label
   */
  protected def createLabel( text: String ) = {
    val retval = new JLabel( text )
    retval.setAlignmentX( 0.0f )
    retval
  }

  /**
   * Creates the description text panel.
   * @param initial The initial text value
   * @return A text area component that can hold descriptions
   */
  protected def createDescription( initial: String ) = {
    val retval = new JTextArea( initial )
    retval.setAlignmentX( 0.0f )
    retval.setLineWrap( true )
    retval
  }

  /**
   * Sets the value of the output description label.
   * Note that this will append OUTPUT_DESC_PREFIX to the text shown, unless
   * the value we set to is an empty string
   * @param value The value to set the output description to
   */
  def setOutputDescription( value: String ) {
    var newValue: String = null

    if ( value == "" ) {
      newValue = ""
    } else {
      newValue = DescriptionPanel.OUTPUT_DESC_PREFIX + value
    }
    
    outputLabel.setText( newValue )
  }

  /**
   * Shows a description of the given describable thing.
   * @param item The describable item
   */
  def showDescription( item: Describable ) {
    showDescription( item.describer )
  }

  /**
   * Shows a description for the given describable thing.
   * @param item The item to show a description of
   */
  def showDescription( item: Description ) {
    name.setText( item.name )
    desc.setText( item.description )
    tableModel.changeData( item.detailedDescription )
    setOutputDescription( item.outputDescription )
  }
}

