/*
 * GUIFunction.scala
 * 
 * Version:
 *     $Id: GUIFunction.scala,v 1.12 2011/06/20 22:19:56 kyledewey Exp $
 *
 * Revisions:
 *      $Log: GUIFunction.scala,v $
 *      Revision 1.12  2011/06/20 22:19:56  kyledewey
 *      Now automatically sets a parameter to be used if it is
 *      required.
 *
 *      Revision 1.11  2011/06/19 20:05:51  kyledewey
 *      Refactored PreFunctionParams.
 *      PreFunctionParam code moved to PreFunctionParam.scala.
 *
 *      Revision 1.10  2011/05/31 18:47:41  kyledewey
 *      Moved showError to ErrorShower.
 *      Now implements ErrorShower.
 *
 *      Revision 1.9  2011/05/31 00:04:31  kyledewey
 *      Moved TypeHolder to sentinel.utils.interactive.
 *      Moved type-related functions to sentinel.utils.interactive.
 *      Moved code that moves rows in response to mouse
 *      presses to sentinel.utils.interactive.  Refactored
 *      such code to be applicable to arbitrary tables.
 *
 *      Revision 1.8  2011/04/11 22:49:04  kyledewey
 *      Moved getParentJFrame's bulk to GUIHelpers.
 *
 *      Revision 1.7  2011/04/10 04:29:54  kyledewey
 *      Added support for the cancel button.
 *      Creating a function will close the "Create Function" panel.
 *      Refactored so actions are dispatched in constant time.
 *
 *      Revision 1.6  2011/04/10 04:06:46  kyledewey
 *      Moved many static routins to GUIHelpers.
 *      Implemented createFunction().
 *
 *      Revision 1.5  2011/04/08 00:18:52  kyledewey
 *      Removed debugging statements.
 *
 *      Revision 1.4  2011/04/04 00:21:30  kyledewey
 *      Now moving rows is based only on what row one is pointing
 *      to.
 *
 *      Revision 1.3  2011/04/03 21:17:02  kyledewey
 *      Added the ability to move params.
 *
 *      Revision 1.2  2011/04/03 04:18:02  kyledewey
 *      Added the "Data" type for type combo boxes, and
 *      the current data type is automatically selected.
 *
 *      Revision 1.1  2011/04/01 03:24:44  kyledewey
 *      Initial revision
 *
 *
 */

package sentinel.vpl

import javax.swing._
import javax.swing.event._
import javax.swing.table._
import java.awt._
import java.awt.event._

import sentinel.model._
import ParamType._
import sentinel.utils.interactive._
import TypeConversions._

/**
 * Holds constants and static routines for GUIFunction.
 * @author Kyle Dewey
 */
object GUIFunction {
  import GUIHelpers._
  import ConversionValidator._

  // text for buttons
  val CANCEL_BUTTON_TEXT = "Cancel"
  val CREATE_FUNCTION_BUTTON_TEXT = "Create Function"

  /**
   * Given an isArray value, it will make a check box
   * @param isArray The isArray value
   * @return A check box holding the possible transformations
   */
  def makeIsArrayCheckBox( isArray: Boolean ) =
    makeCheckBox( isArray,
		  convertableIsArray( isArray ) )

  /**
   * Given an isRequired value, it will make a check box
   * @param isRequired The isRequired value
   * @return A check box holding the possible transformations
   */
  def makeIsRequiredCheckBox( isRequired: Boolean ) =
    makeCheckBox( isRequired,
		  convertableIsRequired( isRequired ) )
  
  /**
   * Given an isRequired value, it will make a check box for
   * the isUsed value.
   * @param isRequired The isRequired value
   * @return A check box holding the possible transformations
   */
  def makeIsUsedCheckBox( isRequired: Boolean ) =
    makeCheckBox( isRequired,
		  convertableIsUsed( isRequired ) )

  /**
   * Makes the check boxes for all the param infos in the pre function
   * for the isArray value
   * @param preFunction The prefunction
   * @return A check box for each param info object
   */
  def makeIsArrayCheckBoxes( preFunction: PreFunction ) =
    preFunction.mapParamInfos( ( paramInfo: ParamInfo ) =>
      makeIsArrayCheckBox( paramInfo.isArray ) )
  
  /**
   * Makes the check boxes for all the param infos in the pre function
   * for the isRequired value
   * @param preFunction The prefunction
   * @return A check box for each param info object
   */
  def makeIsRequiredCheckBoxes( preFunction: PreFunction ) =
    preFunction.mapParamInfos( ( paramInfo: ParamInfo ) =>
      makeIsRequiredCheckBox( paramInfo.isRequired ) )

  /**
   * Makes the check boxes for all the param infos in the pre function
   * for the isUsed value
   * @param preFunction The prefunction
   * @return A check box for each param info object
   */
  def makeIsUsedCheckBoxes( preFunction: PreFunction ) =
    preFunction.mapParamInfos( ( paramInfo: ParamInfo ) =>
      makeIsUsedCheckBox( paramInfo.isRequired ) )
}

/**
 * Holds constants and static routines for ParameterTableModel.
 * @author Kyle Dewey
 */
object ParameterTableModel {
  // holds the names of all columns
  val COLUMN_NAMES = Array[ Object ]( "Name", 
		 		      "Description",
				      "Type",
				      "Array?",
				      "Required?",
				      "Used?" )
  val NAME_COLUMN = 0
  val DESC_COLUMN = 1
  val TYPE_COLUMN = 2
  val IS_ARRAY_COLUMN = 3
  val IS_REQUIRED_COLUMN = 4
  val IS_USED_COLUMN = 5
  
  /**
   * Makes a row of data for the given ParamInfo object.
   * @param param The pre funciton param associated with the object
   * @return The row of data
   */
  def makeRowData( param: PreFunctionParam ) = {
    val retval: Array[ Object ] = new Array( COLUMN_NAMES.length )
    retval( NAME_COLUMN ) = param.currentName
    retval( DESC_COLUMN ) = param.currentDesc
    retval( TYPE_COLUMN ) = TypeHolder( param.currentType )
    retval( IS_ARRAY_COLUMN ) = boolean2Boolean( param.currentIsArray )
    retval( IS_REQUIRED_COLUMN ) = boolean2Boolean( param.currentIsRequired )
    retval( IS_USED_COLUMN ) = boolean2Boolean( param.currentIsUsed )
    retval
  }

  /**
   * Generates data for the model.
   * @param preFunction The prefunction associated with the data
   * @return A matrix of data
   */
  def makeData( preFunction: PreFunction ) = 
    preFunction.mapPreFunctionParams( makeRowData( _ ) ).toArray

  /**
   * Converts the given object to java.lang.Boolean and
   * returns the value
   * @param item The item to convert
   * @return the boolean value
   */
  def toBool( item: Object ) =
    item.asInstanceOf[ java.lang.Boolean ]
        .booleanValue
}
  
/**
 * Table model for parameters.
 * @param gui The gui function that is associated with this model
 * @author Kyle Dewey
 */
class ParameterTableModel( val gui: GUIFunction ) 
extends DefaultTableModel( ParameterTableModel.makeData( gui.preFunction ),
			   ParameterTableModel.COLUMN_NAMES ) with TableModelListener {
  import ParameterTableModel._
  import ParamType._

  // begin instance variables

  // names of each parameter.
  // as this is mutable, we need to know the original names to
  // change a parameter name
  private var oldNames = names

  // maps column numbers with functions that can manipulate
  // data in the column
  val columnManipulators: Map[ Int, Int => Unit ] = 
    Map( NAME_COLUMN -> nameChanged,
	 DESC_COLUMN -> descChanged,
	 TYPE_COLUMN -> typeChanged,
	 IS_ARRAY_COLUMN -> isArrayChanged,
	 IS_REQUIRED_COLUMN -> isRequiredChanged,
	 IS_USED_COLUMN -> isUsedChanged )
  // end instance variables

  // begin constructor
  addTableModelListener( this )
  // end constructor

  /**
   * Gets the names of all parmeters.
   * @return All the names, where retval( 0 ) is the first row's
   * name, retval( 1 ) is the second, etc.
   */
  def names() = 
    0.until( getRowCount )
     .map( name( _ ) )
     .toArray

  /**
   * Gets the old name for the given row
   * @param row The row
   * @return The name for the row
   */
  def oldName( row: Int ) =
    oldNames( row )

  /**
   * Gets the current name for the given row
   * @param row The row
   * @return The name of the row
   */
  def name( row: Int ) =
    getString( row, NAME_COLUMN )

  /**
   * Gets the given item as a string.
   * @param row The row the item is at
   * @param column The column the item is at
   * @return The item as a string
   */
  def getString( row: Int, column: Int ) =
    getValueAt( row, column ).asInstanceOf[ String ]

  /**
   * Gets the given item as a type
   * @param row The row the item is at
   * @param column The column the item is at
   * @return The item as a ParamType
   */
  def getType( row: Int, column: Int ): ParamType =
    getValueAt( row, column ).asInstanceOf[ TypeHolder ].theType

  /**
   * Gets the description for the given row
   * @param row The row
   * @return The description for the row
   */
  def desc( row: Int ) =
    getString( row, DESC_COLUMN )

  /**
   * Gets the type for the given row
   * @param row The row
   * @return The type for this row
   */
  def getType( row: Int ): ParamType =
    getType( row, TYPE_COLUMN )

  /**
   * Gets a boolean from the given row and column
   * @param row The row
   * @param column The column
   * @return A boolean here
   */
  def getBoolean( row: Int, column: Int ) =
    ParameterTableModel.toBool( getValueAt( row, column ) )

  /**
   * Gets whether or not the parameter is an array for
   * the given row.
   * @param row The row
   * @return Whether or not it's an array
   */
  def isArray( row: Int ) =
    getBoolean( row, IS_ARRAY_COLUMN )

  /**
   * Gets whether or not the parameter is required for
   * the given row.
   * @param row The row
   * @return Whether or not it's required
   */
  def isRequired( row: Int ) =
    getBoolean( row, IS_REQUIRED_COLUMN )

  /**
   * Gets whether or not the parameter is used for
   * the given row
   * @param row The row
   * @return Whether or not it's used
   */
  def isUsed( row: Int ) =
    getBoolean( row, IS_USED_COLUMN )

  /**
   * Indicates that the name column has been changed
   * @param row The row that has changed
   * @throws ParameterNameRepeatException If an attempt is made to
   * change to an existing name
   * @throws UnknownParameterNameException If the given parameter
   * name isn't recognized
   */
  def nameChanged( row: Int ) {
    val newName = name( row )
    
    gui.preFunction.changeParameterName( oldName( row ),
					 newName )
    oldNames( row ) = newName
  }

  /**
   * Indicates that the description column has been changed
   * @param row The row that has changed
   * @throws UnknownParameterNameException If the given parameter
   * name isn't recognized
   */
  def descChanged( row: Int ) {
    gui.preFunction.changeParameterDescription( name( row ),
					        desc( row ) )
  }

  /**
   * Indicates that the type column has changed
   * @param row The row that has changed
   * @throws UnknownParameterNameException If the given parameter
   * name isn't recognized
   * @throws ParameterTypeChangeException If the type change is invalid
   */
  def typeChanged( row: Int ) {
    gui.preFunction.changeParameterType( name( row ),
					 getType( row ) )
  }

  /**
   * Indicates that the value of isArray has changed
   * @param row The row that has changed
   * @throws UnknownParameterNameException If the given parameter
   * name isn't recognized
   * @throws ParameterArrayChangeException If the array change is invalid
   */
  def isArrayChanged( row: Int ) {
    gui.preFunction.changeParameterIsArray( name( row ),
					    isArray( row ) )
  }

  /**
   * Indicates that the value of isRequired has changed
   * @param row The row that has changed
   * @throws UnknownParameterNameException If the given parameter
   * name isn't recognized
   * @throws ParameterRequiredException If the requirement change
   * is invalid
   */
  def isRequiredChanged( row: Int ) {
    gui.preFunction.changeParameterIsRequired( name( row ),
					       isRequired( row ) ) 
  }

  /**
   * Indicates that the value of isUsed has changed
   * @param row The row that has changed
   * @throws UnknownParameterNameException If the given parameter
   * name isn't recognized
   * @throws ParameterUsedException If the parameter isn't optional and
   * we don't want to use it
   */
  def isUsedChanged( row: Int ) {
    gui.preFunction.changeParameterIsUsed( name( row ),
					   isUsed( row ) )
  }

  /**
   * Indicates that the given row and column has changed.
   * @param row The row
   * @param column The column
   */
  def tableChanged( row: Int, column: Int ) {
    try {
      columnManipulators( column )( row )
    } catch {
      case e: ParameterNameRepeatException => showError( e )
      case e: UnknownParameterNameException => showError( e )
      case e: ParameterTypeChangeException => showError( e )
      case e: ParameterArrayChangeException => showError( e )
      case e: ParameterRequiredException => showError( e )
      case e: ParameterUsedException => showError( e )
    }
  }

  /**
   * Called when a value in the table changes.
   * @param event The event that correlates to the change
   */
  def tableChanged( event: TableModelEvent ) {
    if ( event.getFirstRow == event.getLastRow &&
         event.getType == TableModelEvent.UPDATE ) {
      // changing a single element
      tableChanged( event.getFirstRow,
		    event.getColumn )
    } else {
      // moving rows
      oldNames = names
    }
  }

    
  /**
   * Shows an error message for the given exception.
   * Merely calls <code>gui.showError</code>
   * @param e The exception to show an error for
   */
  def showError( e: Exception ) {
    gui.showError( e )
  }

  /**
   * Moves a series of rows in the table.
   * @param start The start of the rows
   * @param end The end of the rows
   * @param to Where to put the rows.
   */
  override def moveRow( start: Int,
		        end: Int,
		        to: Int ) {
    super.moveRow( start,
		   end,
		   to )
    gui.preFunction.moveParams( start,
			        end,
			        to )
  }

  /**
   * Moves a single row in the table.
   * @param row The row to move
   * @param to Where to move the row
   */
  def moveRow( row: Int, to: Int ) {
    moveRow( row, row, to )
  }
}

/**
 * Holds static helper routines for ParameterTable.
 * @author Kyle Dewey
 */
object ParameterTable {
  import GUIFunction._

  /**
   * Makes information by row for a specific complex type.
   * @param preFunction The prefunction
   * @param componentMaker Something that can make components from the
   * preFunction
   * @param editorMaker Something that can make cell editors
   * based on the components
   * @param rendererMaker Something that can make renderers based on
   * the components
   * @return Tuples by row consisting of the component, the editor,
   * and the renderer
   */
  def makeByRow[ T, U, V ]( preFunction: PreFunction,
			    componentMaker: PreFunction => Seq[ T ],
			    editorMaker: Seq[ T ] => Seq[ U ],
			    rendererMaker: Seq[ T ] => Seq[ V ] ): 
  Array[ Tuple3[ T, U, V ] ] = {
    val components = componentMaker( preFunction )
    val editors = editorMaker( components )
    val renderers = rendererMaker( components )
    (components, editors, renderers).zipped.map(
      ( component,
        editor,
        renderer ) =>
	 Tuple3( component,
		 editor,
		 renderer ) ).toArray
  }

  /**
   * Like <code>makeByRow</code>, but it uses <code>renderers</code> for the
   * <code>rendererMaker</code> param.
   * @param preFunction The pre function
   * @param componentMaker Something that can make components from the
   * preFunction
   * @param editorMaker Something that can make cell editors
   * based on the components
   * @return Components, cell editors, and cell renderers by row
   */
  def makeByRow[ T <: Component, U ]( preFunction: PreFunction,
				      componentMaker: PreFunction => Seq[ T ],
				      editorMaker: Seq[ T ] => Seq[ U ] ): 
  Array[ Tuple3[ T, U, TableCellRenderer ] ] = {
    makeByRow( preFunction,
	       componentMaker,
	       editorMaker,
	       GUIHelpers.renderers( _ ) )
  }
  
  /**
   * Like <code>makeByRow</code>, but it uses
   * <code>makeCheckCellEditors</code> for the <code>editorMaker</code>
   * param.
   * @param preFunction The prefunction
   * @param componentMaker Makes components
   * @return check boxes, cell editors, and cell renderers by row
   */
  def makeByRow[ T <: JCheckBox ]( preFunction: PreFunction,
				   componentMaker: PreFunction => Seq[ T ] ): 
  Array[ Tuple3[ T, TableCellEditor, TableCellRenderer ] ] = {
    makeByRow( preFunction,
	       componentMaker,
	       GUIHelpers.makeCheckCellEditors( _ ) )
  }

  /**
   * Makes type combo boxes based on the given prefunction.
   * @param preFunction The prefunction
   * @return Combo boxes for each of the ParamInfos within
   */
  def makeTypeComboBoxes( preFunction: PreFunction ): Seq[ JComboBox[TypeHolder] ] =
    TypeConversions.makeTypeComboBoxes( 
      preFunction.mapParamInfos( p => p ) )

  /**
   * Makes type information by row.
   * @param preFunction the prefunction
   * @return Combo boxes, cell editors, and cell renderers by row
   */
  def makeTypeByRow( preFunction: PreFunction ) =
    makeByRow( preFunction,
	       makeTypeComboBoxes( _ ),
	       ( p: Seq[ JComboBox[TypeHolder] ] ) => GUIHelpers.makeComboCellEditors( p ) )
  
  /**
   * Makes isArray information by row
   * @param preFunction The prefunction
   * @return check boxes, cell editors, and cell renderers by row
   */
  def makeIsArrayByRow( preFunction: PreFunction ) =
    makeByRow( preFunction,
	       makeIsArrayCheckBoxes( _ ) )

  /**
   * Makes isRequired information by row
   * @param preFunction The prefunction
   * @return check boxes, cell editors, and cell renderers by row
   */
  def makeIsRequiredByRow( preFunction: PreFunction ) =
    makeByRow( preFunction,
	       makeIsRequiredCheckBoxes( _ ) )

  /**
   * Makes isUsed information by row
   * @param preFunction The prefunction
   * @return check boxes, cell editors, and cell renderers by row
   */
  def makeIsUsedByRow( preFunction: PreFunction ) =
    makeByRow( preFunction,
	       makeIsUsedCheckBoxes( _ ) )
}

/**
 * Holds helper routines for IsRequiredEditorListener
 * @author Kyle Dewey
 */
object IsRequiredEditorListener {
  /**
   * Gets the current isRequired value, as shown by the given editor.
   * Assumes that the given editor is that of a JCheckBox.
   * @param editor The editor
   * @return The current isRequired value, as shown by the given editor
   */
  def checkBoxValue( editor: Object ) =
    editor.asInstanceOf[ DefaultCellEditor ]
          .getComponent
	  .asInstanceOf[ JCheckBox ]
	  .isSelected

  /**
   * Enables/disables the underlying component of the given cell editor
   * @param editor The cell editor
   * @param enable Whether to enable the component or disable it
   */
  def setEnabledComponent( editor: DefaultCellEditor, enable: Boolean ) {
    editor.getComponent.setEnabled( enable )
  }

  /**
   * Treats the given cell editor as that of a JCheckBox.
   * Programatically sets the value of the editor.
   * @param editor The cell editor
   * @param value The value to set the editor to
   */
  def setValueComponent( editor: DefaultCellEditor, value: Boolean ) {
    editor.getComponent.asInstanceOf[ JCheckBox ].setSelected( value )
    editor.stopCellEditing()
  }

  /**
   * Treats the given cell editor is that of isUsed.
   * Sets it and enables/disables it based upon the current value of isRequired.
   * @param editor The isUsed cell editor
   * @param isRequired The current value of isRequired
   */
  def setIsUsed( editor: DefaultCellEditor, isRequired: Boolean ) {
    if ( isRequired ) {
      setValueComponent( editor, true )
      setEnabledComponent( editor, false )
    } else {
      setEnabledComponent( editor, true )
    }
  }
}

/**
 * A cell editor listener for the isRequired column.
 * @param isUsedEditor The editor for the isUsed column for this row
 * @param table The Parameter Table that we are associated with
 * @author Kyle Dewey
 */
class IsRequiredEditorListener( val isUsedEditor: DefaultCellEditor,
			        val table: ParameterTable ) 
extends CellEditorListener {
  import IsRequiredEditorListener._

  /**
   * Signals that editing has been cancelled.
   * Doesn't actually do anything.
   * @param event The change event associated with the cancel
   */
  def editingCanceled( event: ChangeEvent ) {}

  /**
   * Signals that editing has stopped.
   * Based on what isRequired has done, we need to change isUsed.
   * @param event The change event associated with the stop
   */
  def editingStopped( event: ChangeEvent ) {
    setIsUsed( isUsedEditor,
	       checkBoxValue( event.getSource ) )
    table.setValueAt( boolean2Boolean( checkBoxValue( isUsedEditor ) ),
		      table.isUsedEditorRow( isUsedEditor ),
		      ParameterTableModel.IS_USED_COLUMN )
  }
}

/**
 * Table for parameters.
 * @param gui The gui function that is associated with this model
 * @author Kyle Dewey
 */
class ParameterTable( val gui: GUIFunction ) 
extends JTable( new ParameterTableModel( gui ) ) with RowMover {
  import GUIFunction._
  import ParameterTableModel._
  import ParameterTable._

  // begin instance variables
  // cell editors and renderers by row
  private var typeByRow = makeTypeByRow( gui.preFunction )
  private var isArrayByRow = makeIsArrayByRow( gui.preFunction )
  private var isRequiredByRow = makeIsRequiredByRow( gui.preFunction )
  private var isUsedByRow = makeIsUsedByRow( gui.preFunction )

  // the row that is currently being dragged
  private var rowDragging: Option[ Int ] = None
  // end instance variables

  // begin constructor
  // selection stuff
  setSelectionMode( ListSelectionModel.SINGLE_SELECTION )

  // cell editor listeners for isRequired
  setupCellListeners()
  // end constructor

  /**
   * Gets the row that the given isUsed cell editor is associated with.
   * @param editor The editor
   * @return The row that the editor is associated with, or -1 if there
   * is no such editor in the table.
   */
  def isUsedEditorRow( editor: DefaultCellEditor ) = 
    isUsedByRow.indexWhere( _._2.eq( editor ) )

  /**
   * Sets up the cell editor listeners for isRequired.
   */
  protected def setupCellListeners() {
    0.until( isRequiredByRow.length ).foreach( index =>
      isRequiredByRow( index )._2.addCellEditorListener( 
	new IsRequiredEditorListener( 
	  isUsedByRow( index )._2.asInstanceOf[ DefaultCellEditor ], this ) ) )
  }

  /**
   * Gets the cell editor for the given row and column.
   * @param row The row
   * @param column The column
   * @return The cell editor for the given row and column
   */
  override def getCellEditor( row: Int, column: Int ) =
    column match {
      case TYPE_COLUMN => typeByRow( row )._2
      case IS_ARRAY_COLUMN => isArrayByRow( row )._2
      case IS_REQUIRED_COLUMN => isRequiredByRow( row )._2
      case IS_USED_COLUMN => isUsedByRow( row )._2
      case _ => super.getCellEditor( row, column )
    }

  /**
   * Gets the cell renderer for the given row and column.
   * @param row The row
   * @param column The column
   * @return The renderer for the given row and column
   */
  override def getCellRenderer( row: Int, column: Int ) = 
    column match {
      case TYPE_COLUMN => typeByRow( row )._3
      case IS_ARRAY_COLUMN => isArrayByRow( row )._3
      case IS_REQUIRED_COLUMN => isRequiredByRow( row )._3
      case IS_USED_COLUMN => isUsedByRow( row )._3
      case _ => super.getCellRenderer( row, column )
    }

  /**
   * Moves the given rows to the given position.
   * @param start The start of the row subsequence
   * @param end The end of the row subsequence
   * @param to Where to move them to
   */
  def moveRows( start: Int, end: Int, to: Int ) {
    if ( start != to ) {
      def f[ T : scala.reflect.ClassTag ]( seq: Seq[ T ] ) =
	SentinelHelpers.moveSubsequence( start, 
					 end, 
					 to, 
					 seq ).toArray
      typeByRow = f( typeByRow )
      isArrayByRow = f( isArrayByRow )
      isRequiredByRow = f( isRequiredByRow )
      isUsedByRow = f( isUsedByRow )
      getModel.asInstanceOf[ ParameterTableModel ]
              .moveRow( start,
		        end,
		        to )
    }
  }
}

/**
 * A GUI panel for function creation.
 * The panel allows the user to customize elements of functions as desired.
 * @param preFunction The function that is associated with this panel.
 * @param languages Manager holding the languages we can select from
 * @author Kyle Dewey
 */
class GUIFunction( val preFunction: PreFunction,
		   val languages: LanguageManager ) 
extends JPanel with ErrorShower with ActionListener {
  // begin instance variables
  private val name = makeTextField( preFunction.name.getOrElse( null ) )
  private val desc = makeTextField( preFunction.description.getOrElse( null ) )
  private val params = new ParameterTable( this )
  private val cancelButton = makeButton( GUIFunction.CANCEL_BUTTON_TEXT )
  private val createFunctionButton = 
    makeButton( GUIFunction.CREATE_FUNCTION_BUTTON_TEXT )  
  private val nameDescPanel = new JPanel()
  private val languagesCombo = languages.toComboBox
  private val actions: Map[ Object, () => Unit ] = 
    Map( name -> changeName,
	 desc -> changeDesc,
	 cancelButton -> closePanel,
	 createFunctionButton -> createFunction )
  // end instance variables

  // begin constructor
  nameDescPanel.setLayout( new GridLayout( 2, 2 ) )
  nameDescPanel.add( new JLabel( "Name:" ) )
  nameDescPanel.add( name )
  nameDescPanel.add( new JLabel( "Description:" ) )
  nameDescPanel.add( desc )
  add( nameDescPanel )
  add( new JScrollPane( params ) )
  add( cancelButton )
  createFunctionButton.setEnabled( false )
  add( createFunctionButton )
  add( languagesCombo )
  
  /**
   * Creates a text field with the given information.
   * Registers <code>this</code> as an action listener.
   * @param text The intital text to put into the field.  May
   * be null
   * @return A text field holding the given text, with <code>this</code>
   * registered as an action listener.
   */
  protected def makeTextField( text: String ) = {
    val retval = new JTextField( text )
    retval.addActionListener( this )
    retval
  }

  /**
   * Creates a button with the given name.
   * Registers <code>this</code> as an action listener
   * @param name The name of the button
   * @return A button with the given name and <code>this</code>
   * registered as an action listener
   */
  protected def makeButton( name: String ) = {
    val retval = new JButton( name )
    retval.addActionListener( this )
    retval
  }

  /**
   * Called when an action is performed on one of
   * the constituents.
   * @param event The event correlating to the action
   */
  def actionPerformed( event: ActionEvent ) {
    val source = event.getSource
    
    if ( actions.contains( source ) ) {
      actions( source )()
      tryEnableCreateFunction()
    }
  }

  /**
   * Enables the create function button iff we can create a function.
   */
  def tryEnableCreateFunction() {
    if ( preFunction.canMakeInstanceFactory.isEmpty ) {
      createFunctionButton.setEnabled( true )
    }
  }

  /**
   * Called when the name has been changed
   */
  def changeName() {
    preFunction.name = GUIHelpers.toOption( name.getText )
  }

  /**
   * Called when the description has been changed
   */
  def changeDesc() {
    preFunction.description = GUIHelpers.toOption( desc.getText )
  }

  /**
   * Gets the parent JFrame that this panel is under.
   * @return The parent JFrame.
   */
  def getParentJFrame() = 
    GUIHelpers.getParentJFrame( this )

  /**
   * Closes this panel.
   */
  def closePanel() {
    getParentJFrame match {
      case Some( frame ) => {
	frame.setVisible( false )
	frame.dispose()
      }
      case None => {} // avoids compiler warning about non-exhastive matching
    }
  }

  /**
   * Adds the given function to the appropriate place.
   * This is determined by the combo box.
   * @param function The function
   */
  protected def addFunction( function: InstanceFactory[ _ ] ) {
    try {
      val selected = languagesCombo.getSelectedItem
      if ( selected != null ) {
	selected.asInstanceOf[ LoadedLanguage ]
                .addFunction( function )
      } else {
	languages.addFunction( function )
      }
    } catch {
      case e: FunctionAddException => showError( e )
    }
  }
      
  /**
   * Creates a function.
   * This will register the given function, and add it to the list
   * of functions that we can use.
   */
  def createFunction() {
    try {
      val function = preFunction.instanceFactory
      FactoryManager.registerFactory( function )
      addFunction( function )
    } catch {
      case e: UnknownFactoryTypeException => showError( e )
    } finally {
      closePanel()
    }
  }
}
