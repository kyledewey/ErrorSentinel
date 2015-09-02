/*
 * SentinelTable.scala
 */

package sentinel.utils.interactive

import java.awt._
import java.awt.event._
import javax.swing._
import javax.swing.event._
import javax.swing.table._

import sentinel.model._

/**
 * Holds constants and helper routines pertaining to SentinelTable.
 * @author Kyle Dewey
 */
object SentinelTable {
  // colors for different kinds of cells
  val CLEAN_COLOR = Color.GREEN
  val CORRECTABLE_COLOR = Color.YELLOW
  val ERROR_COLOR = Color.RED
  val BACKGROUND_COLOR = Color.WHITE
  val GRID_LINE_COLOR = Color.BLACK
  val VALID_CELL_COLORS = Set( CLEAN_COLOR,
			       CORRECTABLE_COLOR,
			       ERROR_COLOR )

  // text for different error conditions
  val CLEAN_TEXT = "OK"
  val UNKNOWN_ERROR_TEXT = "Unknown error in error correction language."

  // text for different menu actions
  val CORRECTION_PREFIX = "Correct to \""
  val CORRECTION_POSTFIX = "\""
  val ADD_ROW_ABOVE = "Add Row Above"
  val ADD_ROW_BELOW = "Add Row Below"
  val REMOVE_ROW = "Remove Row"

  // number of millisenconds between key presses to generate a new row
  val NUM_MILLIS_BETWEEN_NEW_ROW = 200

  // registry of the open tables, by name
  var openTables: Map[ String, SentinelTable ] = Map()

  /**
   * Calls the given function if the given name is that of a sheet.
   * @param name The name of the sheet
   * @param function The function to call with the sheet
   * @return Whatever the function returned, or None if the given sheet
   * doesn't exist.
   */
  def callIfSheet[ T ]( name: String, function: SentinelTable => T ) = 
    if ( openTables.contains( name ) ) {
      Some( function( openTables( name ) ) )
    } else {
      None
    }

  /**
   * Makes the given text look like a correction item
   * @param text The text to manipulate
   * @return The text looking like a correction item
   */
  def makeLookLikeCorrectionOption( text: String ) =
    CORRECTION_PREFIX + text + CORRECTION_POSTFIX

  /**
   * Determines if the given text looks like a correction item
   * @param text The text to check
   * @return true if it looks like a correction item, else false
   */
  def looksLikeCorrectionOption( text: String ) =
    hasPrefixPostfix( text, CORRECTION_PREFIX, CORRECTION_POSTFIX )

  /**
   * Gets whether or not the given text begins with the given prefix and
   * ends with the given postfix.
   * @param text The text to check
   * @param prefix The prefix to check
   * @param postfix The postfix to check
   * @return True if it has the given prefix and postfix, else false.
   */
  def hasPrefixPostfix( text: String, prefix: String, postfix: String ) =
    text.startsWith( prefix ) && text.endsWith( postfix )

  /**
   * Extracts the inner text out given a certain prefix and postfix.
   * @param text The text to extract from
   * @param prefix The prefix for the text
   * @param postfix The postfix for the text
   * @return The extracted text, or None if it didn't have the given prefix
   * and postfix
   */
  def extractInner( text: String, prefix: String, postfix: String ) =
    if ( hasPrefixPostfix( text, prefix, postfix ) ) {
      Some( text.substring( prefix.length, text.length - postfix.length ) )
    } else {
      None
    }

  /**
   * Gets the correction option text from something.
   * @param text The text to get the correction option text from
   * @return The text if it looks like a correction option, or None if
   * it doesn't look like a correction option.
   */
  def correctionOptionCorrection( text: String ) =
    extractInner( text, CORRECTION_PREFIX, CORRECTION_POSTFIX )

  /**
   * Makes a menu item with the given text that has the given
   * action listener associated.
   * @param text The text of the menu item
   * @param listener The action listener that can listen for events from
   * the item
   * @return A new menu item that shows the given text that has the given
   * action listener attached
   */
  def menuItem( text: String, listener: ActionListener ) = {
    val retval = new JMenuItem( text )
    retval.addActionListener( listener )
    retval
  }

  /**
   * Gets the replacement text from the given instance result.
   * @param instanceResult The instance result
   * @return The text of the replacement, or None if there is no such text
   */
  def replacement( instanceResult: Option[ Option[ InstanceResult ] ] ) = 
    if ( instanceResult.isDefined &&
	 instanceResult.get.isDefined &&
	 instanceResult.get.get.isInstanceOf[ InstanceFailureReplacement ] ) {
      Some( instanceResult.get.get.asInstanceOf[ InstanceFailureReplacement ].replacement )
    } else {
      None
    }

  /**
   * Gets the text and color of the text to use for the given instance
   * result.
   * @param instanceResult The instance result
   * @return The text and the color of the text to use, in a pair
   */
  def getTextAndColor( instanceResult: Option[ Option[ InstanceResult ] ] ) = {
    import sentinel.utils.noninteractive._
    var text = UNKNOWN_ERROR_TEXT
    var color = ERROR_COLOR

    if ( instanceResult.isDefined ) {
      val ( clean,
	    unclean,
	    whyUnclean ) = ProjectRunner.processCell( "", instanceResult.get )
      if ( clean.isDefined ) {
	val contents = clean.get
	if ( contents == "" ) {
	  // clean
	  text = CLEAN_TEXT
	  color = CLEAN_COLOR
	} else {
	  // have a replacemnt
	  text = contents
	  color = CORRECTABLE_COLOR
	}
      } else if ( unclean.isDefined &&
	          whyUnclean.isDefined ) {
	// uncorrectable error
	text = whyUnclean.get
	color = ERROR_COLOR
      }
    }
  
    (text, color)
  }

  /**
   * Registers the given table in the registry.
   * Will overwrite any previous association
   * @param table The table to register
   */
  def registerTable( table: SentinelTable ) {
    openTables += (table.name -> table)
  }

  /**
   * Gets all open tables
   * @return all open tables
   */
  def tables() =
    openTables.values.toSeq

  /**
   * Runs the given function for each table.
   * @param function The function to run
   */
  def foreachTable( function: SentinelTable => Unit ) {
    tables.foreach( function( _ ) )
  }

  /**
   * Gets the table with the given name.
   * @param name The name of the table
   * @return The table, or None if there is no table with the given name
   */
  def table( name: String ) =
    openTables.get( name )

  /**
   * Sets the color of the given table at the given cell.
   * If there is no such table, this is a no-op.
   * @param pointer The pointer to the cell
   * @param color The color to make the cell
   */
  def setColor( pointer: CellPointer, color: Color ) {
    if ( openTables.contains( pointer.sheet ) ) {
      openTables( pointer.sheet ).setCellColor( pointer.row,
					        pointer.column,
					        color )
    }
  }

  /**
   * Resets the cell colors of all tables.
   */
  def resetCellColors() {
    foreachTable( _.resetCellColors() )
  }

  /**
   * Makes all tables show cell colors based on the instance
   * directly in a cell.
   */
  def showAllCellColors() {
    foreachTable( _.showAllCellColors() )
  }

  /**
   * Like <code>showAllCellColors</code>, except it factors
   * in all parameters.
   */
  def showAllCellColorsWithParams() {
    foreachTable( _.showAllCellColorsWithParams() )
  }

  /**
   * Given cell pointers along with a color, it will set it
   * so all the given cells will be that color.
   * @param pointers Pointers to cells in possibly more than one table
   * @param color The color to make the given cells in those tables.
   */
  def setColors( pointers: Set[ CellPointer ], color: Color ) {
    // clear out all others
    resetCellColors()

    // individually set colors
    pointers.foreach( setColor( _, color ) )
  }

  /**
   * Chooses the "worst" color of a set of colors.
   * This only is defined for CLEAN_COLOR, CORRECTABLE_COLOR, and
   * ERROR_COLOR; other colors are removed beforehand
   * @param set The set of colors
   * @return The "worst" color in the set
   */
  def worstColor( set: Set[ Color ] ): Color = {
    val intersect =  set.intersect( VALID_CELL_COLORS )
    if ( intersect.contains( ERROR_COLOR ) )
      ERROR_COLOR
    else if ( intersect.contains( CORRECTABLE_COLOR ) )
      CORRECTABLE_COLOR
    else
      CLEAN_COLOR
  }

  /**
   * Determines if the first color is worse than the second color.
   * @param first The first color
   * @param second The second color
   * @return true if the first color is worse, else false.
   */
  def worseColor( first: Color, second: Color ) =
    first != second && worstColor( Set( first, second ) ) == first

}

/**
 * Represents the special JTable used for interactive spreadsheets.
 * @param model The underlying table model for the table
 * @author Kyle Dewey
 */
class SentinelTable( val model: WithReplacementSpreadsheet[ WithReplacementCellContents ] ) extends JTable( model ) with ErrorShower with ContextSensitivePopupTable with RowMover with ActionListener {
  import SentinelTable._

  // begin instance variables
  private var cellColors: Map[ (Int, Int), Color ] = Map()
  val tableCellRenderer = makeTableCellRenderer
  val selectionListener = makeListSelectionListener
  val editorListener = makeCellEditorListener
  // end instance variables

  // begin constructor
  SentinelTable.registerTable( this )
  setGridColor( SentinelTable.GRID_LINE_COLOR )
  setShowGrid( true )
  setShowHorizontalLines( true )
  setShowVerticalLines( true )

  // selection stuff
  rsModel.addListSelectionListener( selectionListener )
  csModel.addListSelectionListener( selectionListener )

  // user input listener stuff
  addKeyListener( makeKeyListener )

  // rendering stuff
  setDefaultRenderer( classOf[ String ], tableCellRenderer )
  setDefaultRenderer( classOf[ Object ], tableCellRenderer )

  // editing stuff
  getDefaultEditor( classOf[ String ] ).addCellEditorListener( editorListener )
  getDefaultEditor( classOf[ Object ] ).addCellEditorListener( editorListener )

  model.resetGraph()
  //model.addTableModelListener( model.makeAssociationGraphListener )
  //model.addTableModelListener( model )
  // end constructor

  /**
   * Returns the name of the underlying model.
   * @return The name of the underlying model
   */
  def name() =
    model.name

  /**
   * Moves the given range of rows to the given position.
   * Updates the selection accordingly.
   * @param start The start of the range
   * @param end The end of the range
   * @param to Where to put the rows.
   */
  def moveRows( start: Int, end: Int, to: Int ) {
    if ( start != to ) {
      model.moveRow( start, end, to )
      updateSelection()
    }
  }

  /**
   * Makes a menu item with the given text that holds <code>this</code>
   * as an <code>ActionListener</code>.
   * @param text The text of the menu item
   * @return The menu item with the given text holding <code>this</code>
   * as an <code>ActionListener</code>
   */
  def menuItem( text: String ) =
    SentinelTable.menuItem( text, this )

  /**
   * Gets the replacement for the instance result at the given
   * row and column, or None if there isn't one.
   * @param row The row
   * @param column The column
   * @return The replacement for this row and column, or None
   * if there isn't one.
   */
  def replacement( row: Int, column: Int ) = 
    SentinelTable.replacement( instanceResult( row, column ) )

  /**
   * Adds a row above the currently selected row.
   * If there isn't currently a selected row, this is a no-op.
   */
  def addRowAboveAction() {
    callIfSelected( ( row, column ) => {
      model.insertRow( row ) 
      setSelectedCell( row + 1, column )
      updateSelection()
    } )
  }

  /**
   * Adds a row below the currently selected row.
   * If there isn't currently a selected row, this is a no-op.
   */
  def addRowBelowAction() {
    callIfSelected( ( row, column ) => 
      model.insertRow( row + 1 ) )
  }

  /**
   * Removes the currently selected row.
   * If there isn't a currently selected row, this is a no-op.
   */
  def removeRowAction() {
    callIfSelected( ( row, column ) => {
      model.removeRow( row )
      val selectRow =
	if ( row >= getRowCount ) {
	  getRowCount - 1
	} else {
	  row
	}
      setSelectedCell( selectRow, column )
      updateSelection()
    } )
  }
  
  /**
   * Sets the currently selected row to the given value.
   * If there isn't currently a value selected, this is a no-op.
   * @param value The value to change it to
   */
  def correctionAction( value: String ) {
    callIfSelected( ( row, column ) => {
      model.setValueAt( value, row, column )
      updateSelection()
    } )
  }

  /**
   * Responds to popup menu commands
   * @param event The event that correlates to a menu command
   */
  def actionPerformed( event: ActionEvent ) {
    event.getActionCommand match {
      case ADD_ROW_ABOVE => addRowAboveAction()
      case ADD_ROW_BELOW => addRowBelowAction()
      case REMOVE_ROW => removeRowAction()
      case s: String if ( looksLikeCorrectionOption( s ) ) => 
	correctionAction( correctionOptionCorrection( s ).get )
    }
  }

  /**
   * Like <code>makePopupMenu</code>, but it works with a given cell.
   * Note that this is guarenteed to return something.
   * @param row The row of the cell
   * @param column The column of the cell
   * @return A context-sensitive JPopupMenu
   */
  override def makePopupMenu( row: Int, column: Int ): Option[ JPopupMenu ] = {
    val retval = new JPopupMenu()
    val replace = replacement( row, column )
    if ( replace.isDefined ) {
      retval.add( menuItem( makeLookLikeCorrectionOption( replace.get ) ) )
      retval.addSeparator()
    }
    retval.add( menuItem( REMOVE_ROW ) )
    retval.addSeparator()
    retval.add( menuItem( ADD_ROW_ABOVE ) )
    retval.add( menuItem( ADD_ROW_BELOW ) )
    Some( retval )
  }

  /**
   * Makes a key listener for this table.
   * When the user hits the down key in rapid succession, a new
   * row is added to the table.
   * @return A new key listener fitting the above description
   */
  protected def makeKeyListener() = 
    new KeyAdapter() {
      var lastDown: Long = -1 // when down was last pressed
      override def keyPressed( event: KeyEvent ) {
	val selectedRow = getSelectedRow
	if ( ( event.getKeyCode == KeyEvent.VK_KP_DOWN ||
	       event.getKeyCode == KeyEvent.VK_DOWN ) &&
	     selectedRow != -1 &&
	     selectedRow == getRowCount - 1 ) {
	  val when = event.getWhen
	  if ( lastDown != -1 &&
	       when - lastDown <= NUM_MILLIS_BETWEEN_NEW_ROW ) {
	    model.addRow()
	  }
	  lastDown = when
	}
      }
    }
  
  /**
   * Makes a cell editor listener for this table.
   * This is relevant for when the selection doesn't change but
   * the contents of a cell have.
   * @return A cell editor listener that will update such changes
   */
  protected def makeCellEditorListener() =
    new CellEditorListener() {
      def editingCanceled( e: ChangeEvent ) {
	// note that in this case data hasn't changed, so nothing
	// needs to be updated
      }
      def editingStopped( e: ChangeEvent ) {
	// data has changed
	updateSelection()
      }
    }
  
  /**
   * Makes the list selection listener for this table.
   * @return The list selection listener for this table
   */
  protected def makeListSelectionListener() =
    new ListSelectionListener() {
      def valueChanged( event: ListSelectionEvent ) {
	updateSelection()
      }
    }

  /**
   * Makes the table cell renderer for this table.
   * @return The table cell renderer for this table.
   */
  protected def makeTableCellRenderer() =
    new DefaultTableCellRenderer() {
      /**
       * Gets a renderer for the given cell.
       * This is needed to determine the background color of cells.
       * Note that it will use the default renderer for the base of components.
       * @param table The table that the rendering
       * @param value The value that we are putting in
       * @param isSelected Whether or not it is selected.  Note that with
       * our selection models, interesting enough, it seems that isSelected
       * is always false, even if we have something selected.
       * @param hasFocus If the component has focus
       * @param row The row the component is for
       * @param column The column the component is for
       * @return The component used to show the cell.
       */
      override def getTableCellRendererComponent( table: JTable,
						  value: Object,
						  isSelected: Boolean,
						  hasFocus: Boolean,
						  row: Int,
						  column: Int ) = {
	val asPair = (row, column)
	val baseComponent =
	  super.getTableCellRendererComponent( table, 
					       value,
					       isSelected,
 					       hasFocus,
					       row,
					       column )
	val color = 
	  if ( cellColors.contains( asPair ) ) {
	    cellColors( asPair )
	  } else {
	    SentinelTable.BACKGROUND_COLOR
	  }
	baseComponent.setBackground( color )
	
	baseComponent
      }
    }

  /**
   * Converts the given row, column pairs to cell pointers.
   * The name of the pointer is this table.
   * @param pairs The row, column pairs
   * @return Cell pointers holding the same information
   */
  def pairsToPointers( pairs: Seq[ (Int, Int) ] ) =
    Set() ++ pairs.map( pair =>
      new CellPointer( name, pair._1, pair._2 ) )

  /**
   * To be called when cell colors should have changed.
   * This will make visible any changes.
   * Note that changed can propagate beyond this sheet!
   * Also note that colors are in relation to a given cell
   * @param row The row to change colors with respect to
   * @param column The column to change colors with respect to
   */
  def updateCellColors( row: Int, column: Int ) {
    val ( pairs, 
	  color ) = determineCellColors( row, column )
    SentinelTable.setColors( pairsToPointers( pairs.toSeq ), color )
  }

  /**
   * Updates the text in the information panel based on the
   * instance at the given row and column
   * @param row The row that has been selected
   * @param column The column that has been selected
   */
  def updateInfoText( row: Int, column: Int ) {
    val ( text,
	  color ) = 
	    SentinelTable.getTextAndColor( instanceResult( row, column ) )
    SentinelInformationPanel.showText( text, color )
  }

  /**
   * Updates data based on the given row and column being selected.
   * @param row The row of the selection
   * @param column The column of the selection
   */
  def updateSelection( row: Int, column: Int ) {
    updateCellColors( row, column )
    updateInfoText( row, column )
  }
   
  /**
   * To be called when the selection is believed to have changed.
   * If there isn't currently a selected row or column, this
   * won't do anything.
   */
  def updateSelection() {
    if ( model.shouldRespond ) {
      callIfSelected( updateSelection( _, _ ) )
    }
  }

  /**
   * Gets the instance result for the given row and column
   * @param row The row
   * @param column The column
   * @return The instance result here
   */
  def instanceResult( row: Int, column: Int ) =
    model.getInstanceResult( row, column )

  /**
   * Determines the color of the given cell based on it's instance result
   * @param row The row
   * @param column The column
   * @return The color to use
   */
  def cellColor( row: Int, column: Int ) = {
    val rawInstance = instanceResult( row, column )

    if ( rawInstance.isDefined &&
	 rawInstance.get.isDefined ) {
      val instance = rawInstance.get.get
      if ( instance.success ) {
	SentinelTable.CLEAN_COLOR
      } else if ( instance.isInstanceOf[ InstanceFailureReplacement ] ) {
	SentinelTable.CORRECTABLE_COLOR
      } else {
	SentinelTable.ERROR_COLOR
      }
    } else {
      SentinelTable.ERROR_COLOR
    }
  }

  /**
   * Determines the color of the given cell and all cells that are
   * related to this cell.
   * @param row The row of the cell
   * @param column The column of the cell
   * @return A pair holding a set of pointers to modify, and the color
   * to change them to
   */
  def determineCellColors( row: Int, column: Int ) = 
    (model.parameterCells( row, column ),
     cellColor( row, column ))

  /**
   * Resets all the cell colors to be the same as the background color.
   * This is needed to undo the last selection.  Note that this will force
   * the render and will then clear the cell colors.
   */
  def resetCellColors() {
    if ( !cellColors.isEmpty ) { // only for performance
      cellColors = 
	cellColors.transform( ( coord, color ) =>
	  SentinelTable.BACKGROUND_COLOR )
      forceRender()
      cellColors = Map()
    }
  }

  /**
   * Forces all currently colored cells to be renderered.
   * Without this, unselected cells whose color has changed will
   * be updated only when their selection has changed (which in
   * turn changes the color anyway!)
   */
  def forceRender() {
    cellColors.keys.foreach( pair => 
      forceRender( pair._1, pair._2 ) )
  }

  /**
   * Forces the given cell to be renderered.
   * @param row The row of the cell
   * @param column The column of the cell
   */
  def forceRender( row: Int, column: Int ) {
    model.forceRender( row, column )
  }

  /**
   * Sets the given cell to be the given color.
   * Note that this will force the given cell to be rendered.
   * @param row The row of the cell
   * @param column The column of the cell
   * @param color The color of the cell
   */
  def setCellColor( row: Int, column: Int, color: Color ) {
    cellColors += ((row -> column) -> color)
    forceRender( row, column )
  }

  /**
   * Sets all the cell colors given a parallel table of cell colors.
   * @param colorTable The parallel table of cell colors
   */
  def setCellColors( table: Array[ Array[ Color ] ] ) {
    model.foreachRowColumn( ( row, column ) =>
      setCellColor( row, column,
		    table( row )( column ) ) )
  }

  /**
   * Makes a parallel table of the colors that each cell should be.
   * This is without regard to parameters
   * @return A parallel table of cell colors, without regarding parameters
   */
  def colorTable() =
    model.mapTable( cellColor( _, _ ) )

  /**
   * Like <code>colorTable</code>, but it includes parameters.
   * @return A parallel table of cell colors, regarding parameters
   */
  def colorTableWithParams = {
    val table = colorTable
    var changeMade = false

    // determines if the given color is worse 

    // sets the color of the given row and column
    // ignores and association constraints
    def setColor( row: Int, column: Int, color: Color ) {
      val colorHere = table( row )( column )
      if ( SentinelTable.worseColor( color, colorHere ) ) {
	table( row )( column ) = color
        changeMade = true
      }
    }

    // gets the worst color at the given row and column, including
    // associated colors
    def worstColor( row: Int, column: Int ): Color =
      SentinelTable.worstColor( Set() ++ model.relatedCells( row, column ).map( 
	pair => table( pair._1 )( pair._2 ) ) )
    
    // sets the color of the given row and column
    // obeys association constrains
    def setAssociatedColors( row: Int, column: Int ) {
      val worst = worstColor( row, column )
      model.relatedCells( row, column ).foreach( pair =>
	setColor( pair._1, pair._2, worst ) )
    }

    // does a single pass of the table
    def setWorstColorsPass() {
      model.foreachRowColumn( setAssociatedColors( _, _ ) )
    }

    do {
      changeMade = false
      setWorstColorsPass()
    } while( changeMade );

    table
  } // colorTableWithParams
      
  /**
   * Shows the color of each cell based on the instance results
   * that are there.
   */
  def showAllCellColors() {
    setCellColors( colorTable )
  }

  /**
   * Shows all cell colors with parameters involved.
   */
  def showAllCellColorsWithParams() {
    setCellColors( colorTableWithParams )
  }
}
